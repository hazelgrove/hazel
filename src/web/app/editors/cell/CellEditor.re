open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Poly;

/* A "Cell" with user-editable text at the top, and evaluation results at the bottom. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: CodeEditable.Model.t,
    result: EvalResult.Model.t,
  };

  let mk = editor => {
    editor: {
      editor,
      statics: CachedStatics.empty,
      dynamics: Language.Dynamics.Map.empty,
      context_menu: None,
    },
    result: EvalResult.Model.init,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    editor: CodeEditable.Model.persistent,
    result: EvalResult.Model.persistent,
  };

  let persist = (model: t): persistent => {
    editor: model.editor |> CodeEditable.Model.persist,
    result: model.result |> EvalResult.Model.persist,
  };

  let unpersist = (~settings as _=?, {editor, result}: persistent): t => {
    editor: CodeEditable.Model.unpersist(editor),
    result: EvalResult.Model.unpersist(result),
  };

  /* A cell holding a slide's stored source, with an un-run result.
     Slide sources are text-backed zippers (see PersistentZipper). */
  let from_persistent_zipper = (~root, zipper: PersistentZipper.t): persistent => {
    editor: zipper |> Editor.Model.mk_persistent(~root),
    result: EvalResult.Model.init |> EvalResult.Model.persist,
  };

  let to_string = (model: t) => model.editor |> CodeEditable.Model.to_string;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(CodeEditable.Update.t)
    | ResultAction(EvalResult.Update.t);

  let update = (~settings, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor =
        CodeEditable.Update.update(~settings, action, model.editor);
      {
        ...model,
        editor,
      };
    | ResultAction(action) =>
      let updated =
        EvalResult.Update.update(
          ~settings={
            ...settings,
            core: {
              ...settings.core,
              assist: false,
            },
          },
          action,
          model.result,
        );
      /* If the editor has pending_probe_cursor, force recalculation so
         resolve_pending_probe_cursor can run with the new dynamics */
      let needs_recalc =
        Option.is_some(
          model.editor.editor.state.zipper.refractors.pending_probe_cursor,
        );
      {
        ...updated,
        recalculate: updated.recalculate || needs_recalc,
        model: {
          ...model,
          result: updated.model,
        },
      };
    };
  };

  /* the stepping drawer and the proof drawers take the rows their
     steppers need; true when that changed, which the layout must pick up */
  let fit_drawers =
      (~settings, result: EvalResult.Model.t, editor: CodeEditable.Model.t)
      : bool => {
    let z = editor.editor.state.zipper;
    let steps =
      switch (z.refractors.stepping, result.probe_steps) {
      | (Some(span), Some(ps))
          when Language.Sample.equal_span_ref(ps.span, span) =>
        Haz3lcore.DrawerFit.set_computed(
          span.probe_id,
          ProbeSteps.rows(~settings, ps),
        )
      | _ => false
      };
    let proofs =
      !Id.Map.is_empty(z.refractors.proofs)
      && Theorems.fit(~settings, result.theorems);
    if (steps || proofs) {
      Haz3lcore.ProbeProj.Settings.redraw();
    };
    steps || proofs;
  };

  let calculate =
      (
        ~settings,
        ~autoprobe_mode=Haz3lcore.AutoProbe.Off,
        ~tail_probe=false,
        ~proofs=Haz3lcore.AutoProbePerform.NoProofs,
        ~is_edited,
        ~statics_mode=StaticsMode.Normal,
        ~queue_worker,
        ~stitch,
        ~ana=?,
        {editor, result}: Model.t,
      )
      : Model.t => {
    /* First pass: calculate editor with current dynamics (may be stale) */
    let editor =
      CodeEditable.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~tail_probe,
        ~proofs,
        ~is_edited,
        ~statics_mode,
        ~stitch,
        ~ana?,
        ~dynamics=
          EvalResult.Model.dynamics(
            ~tail=editor.editor.state.zipper.refractors.tail_target,
            result,
          ),
        ~is_dynamic_term=false,
        editor,
      );
    /* Save probe results reference before result calculation */
    let probes_before = EvalResult.Model.probe_results(result);
    /* Calculate result (may produce new dynamics from worker) */
    let result =
      EvalResult.Update.calculate(
        ~settings={
          ...settings,
          assist: false,
        },
        ~queue_worker,
        ~is_edited,
        ~stepping=editor.editor.state.zipper.refractors.stepping,
        editor |> CodeEditable.Model.get_statics,
        result,
      );
    let drawers_resized = fit_drawers(~settings, result, editor);
    /* Detect if dynamics changed (ensures cursor aligns with render-time dynamics).
     * Compare inner maps, not Option wrappers (Option.map creates new Some each call) */
    let probes_after = EvalResult.Model.probe_results(result);
    let dynamics_changed =
      switch (probes_before, probes_after) {
      | (None, None) => false
      | (Some(a), Some(b)) => !phys_equal(a, b)
      | _ => true
      };
    /* Second pass: if there's a pending focus, pending_probe_cursor waiting
       for dynamics, or dynamics changed since the first pass */
    let has_pending_focus =
      Option.is_some(
        editor.editor.state.zipper.refractors.sample_focus.pending_focus,
      );
    let has_pending_cursor =
      Option.is_some(
        editor.editor.state.zipper.refractors.pending_probe_cursor,
      );
    let needs_second_pass =
      has_pending_focus
      || has_pending_cursor
      || dynamics_changed
      || drawers_resized;
    let editor =
      if (needs_second_pass) {
        /* Pass autoprobe_mode to second pass to avoid clear_autoprobe removing the probe */
        CodeEditable.Update.calculate(
          ~settings,
          ~autoprobe_mode,
          ~tail_probe,
          ~proofs,
          ~is_edited=false, /* Not an edit, just resolving pending focus/cursor */
          ~stitch,
          ~ana?,
          ~dynamics=
            EvalResult.Model.dynamics(
              ~tail=editor.editor.state.zipper.refractors.tail_target,
              result,
            ),
          ~is_dynamic_term=false,
          editor,
        );
      } else {
        editor;
      };
    {
      editor,
      result,
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor
    | Result(EvalResult.Selection.t);

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    switch (selection) {
    | MainEditor =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(
          ~inject=a => inject(MainEditor(a)),
          ~selection=(),
          model.editor,
        );
      Update.MainEditor(ci);
    | Result(selection) =>
      let+ ci =
        EvalResult.Selection.get_cursor_info(
          ~inject=a => inject(ResultAction(a)),
          ~selection,
          model.result,
        );
      Update.ResultAction(ci);
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    CodeEditable.Selection.jump_to_tile(tile, model.editor)
    |> Option.map(~f=x => (Update.MainEditor(x), MainEditor));
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~caption: option(Node.t)=?,
        ~result_kind=?,
        ~locked=false,
        ~lines=false,
        /* opt out for cells that are not the viewport-culling scope */
        ~cull=true,
        model: Model.t,
      ) => {
    let (footer, overlays) =
      EvalResult.View.view(
        ~globals={
          ...globals,
          settings: {
            ...globals.settings,
            core: {
              ...globals.settings.core,
              assist: false,
            },
          },
        },
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive(Result(a)))
          | JumpTo(id) =>
            Effect.Many([
              signal(MakeActive(MainEditor)),
              inject(MainEditor(Perform(Move(Goal(TileId(id)))))),
            ]),
        ~inject=a => inject(ResultAction(a)),
        ~selected={
          switch (selected) {
          | Some(Result(a)) => Some(a)
          | _ => None
          };
        },
        ~result_kind?,
        ~locked,
        model.result,
      );
    /* a stepping drawer in this editor renders its result's stepper */
    let (steps_result, steps_inject) = (
      model.result,
      a => inject(ResultAction(a)),
    );
    Haz3lcore.ProbeProj.Settings.steps_view :=
      (
        id =>
          switch (steps_result.probe_steps) {
          | Some(ps) when ps.span.probe_id == id =>
            ProbeSteps.view(
              ~globals,
              ~inject=a => steps_inject(ProbeStepperAction(a)),
              ~focus=f => steps_inject(ProbeStepperFocus(f)),
              ~close=inject(MainEditor(Perform(Probe(HideSteps)))),
              ~stepped=
                switch (
                  Haz3lcore.TermData.segment(
                    ps.span.probe_id,
                    model.editor.editor.syntax.term_data,
                  )
                ) {
                | Some(seg) =>
                  ProbeSteps.one_line(
                    Haz3lcore.Printer.of_segment(
                      ~holes="?",
                      ~indent="",
                      ~is_single_line=true,
                      seg,
                    ),
                  )
                | None => ""
                },
              ps,
            )
          | _ => None
          }
      );
    /* the ⇓ drawer, when the program stopped before its last line */
    let tail = model.editor.editor.state.zipper.refractors.tail_target;
    let stop = EvalResult.Model.stopped(~tail, steps_result);
    Option.iter(tail, ~f=id => EvalResult.Model.note_stop(id, stop));
    Haz3lcore.ProbeProj.Settings.value_fallback :=
      (
        id =>
          Option.equal(Id.equal, tail, Some(id))
            ? Option.map(~f=EvalResult.View.stopped_view(~globals), stop)
            : None
      );
    Haz3lcore.ProofProj.Settings.view :=
      (
        id =>
          Theorems.View.view_one(
            ~globals,
            ~inject=a => steps_inject(TheoremsAction(a)),
            steps_result.theorems,
            id,
          )
      );
    div(
      ~attrs=[
        Attr.classes(
          ["cell", locked ? "locked" : "unlocked"]
          /* dims what the ⇓ drawer kept from the last run */
          @ (
            EvalResult.Model.eval_is_pending(model.result) ? ["running"] : []
          ),
        ),
      ],
      Option.to_list(caption)
      @ [
        CodeEditable.View.view(
          ~globals,
          ~signal=
            locked
              ? _ => Ui_effect.Ignore
              : fun
                | MakeActive =>
                  Ui_effect.Many(
                    [signal(MakeActive(MainEditor))]
                    /* the program takes focus back from a drawer's step */
                    @ (
                      switch (steps_result.probe_steps) {
                      | Some({focus: Some(_), _}) => [
                          steps_inject(ProbeStepperFocus(None)),
                        ]
                      | _ => []
                      }
                    )
                    @ (
                      switch (steps_result.theorems.focus) {
                      | Some(_) => [
                          steps_inject(TheoremsAction(TheoremFocus(None))),
                        ]
                      | None => []
                      }
                    ),
                  ),
          ~edit_mode=
            locked
              ? EditMode.ReadOnly
              : Editable({
                  inject: action => inject(MainEditor(action)),
                  escape: _ => Ui_effect.Ignore,
                  take_focus: _ => Ui_effect.Ignore,
                  focus: selected == Some(MainEditor) ? Some() : None,
                }),
          ~overlays=overlays(model.editor.editor),
          ~lines,
          ~cull,
          ~dynamics=
            EvalResult.Model.dynamics(
              ~tail=model.editor.editor.state.zipper.refractors.tail_target,
              model.result,
            ),
          ~predicted_reuse=EvalResult.Model.predicted_reuse(model.result),
          ~pending_eval_ids=EvalResult.Model.pending_eval_ids(model.result),
          ~show_active_eval=EvalResult.Model.eval_is_pending(model.result),
          model.editor,
        ),
      ]
      @ footer,
    );
  };
};
