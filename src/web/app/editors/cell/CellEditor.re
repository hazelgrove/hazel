open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

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

  let to_string = (model: t) => model.editor |> CodeEditable.Model.to_string;

  let zipper = (model: t) => model.editor.editor.state.zipper;

  let sort = (model: t): Sort.t => CodeEditable.Model.sort(model.editor);
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(CodeEditable.Update.t)
    | ResultAction(EvalResult.Update.t);

  let can_undo = (action: t) => {
    switch (action) {
    | MainEditor(action) => CodeEditable.Update.can_undo(action)
    | ResultAction(action) => EvalResult.Update.can_undo(action)
    };
  };

  let update = (~settings, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor =
        CodeEditable.Update.update(~settings, action, model.editor);
      {
        ...model,
        editor,
      };
    | ResultAction(PromoteExplore(id, code, name, goal, stepper)) =>
      let updated_editor =
        CodeEditable.Update.update(
          ~settings,
          Perform(ReplaceTermWithSource(id, code)),
          model.editor,
        );
      let updated_result =
        EvalResult.Update.update(
          ~settings={
            ...settings,
            core: {
              ...settings.core,
              assist: false,
            },
          },
          PromoteExplore(id, code, name, goal, stepper),
          model.result,
        );
      {
        ...updated_editor,
        model: {
          editor: updated_editor.model,
          result: updated_result.model,
        },
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
        model.editor.editor.state.zipper.refractors.pending_probe_cursor
        != None;
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

  let calculate =
      (
        ~settings,
        ~autoprobe_mode=false,
        ~is_edited,
        ~statics_mode=CodeWithStatics.StaticsNormal,
        ~queue_worker,
        ~stitch,
        {editor, result}: Model.t,
      )
      : Model.t => {
    /* First pass: calculate editor with current dynamics (may be stale) */
    let editor =
      CodeEditable.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~is_edited,
        ~statics_mode,
        ~stitch,
        ~dynamics=EvalResult.Model.dynamics(result),
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
        editor |> CodeEditable.Model.get_statics,
        result,
      );
    /* Detect if dynamics changed (ensures cursor aligns with render-time dynamics).
     * Compare inner maps, not Option wrappers (Option.map creates new Some each call) */
    let probes_after = EvalResult.Model.probe_results(result);
    let dynamics_changed =
      switch (probes_before, probes_after) {
      | (None, None) => false
      | (Some(a), Some(b)) => a !== b
      | _ => true
      };
    /* Second pass: if there's a pending focus, pending_probe_cursor waiting
       for dynamics, or dynamics changed since the first pass */
    let has_pending_focus =
      editor.editor.state.zipper.refractors.sample_focus.pending_focus != None;
    let has_pending_cursor =
      editor.editor.state.zipper.refractors.pending_probe_cursor != None;
    let needs_second_pass =
      has_pending_focus || has_pending_cursor || dynamics_changed;
    let editor =
      if (needs_second_pass) {
        /* Pass autoprobe_mode to second pass to avoid clear_autoprobe removing the probe */
        CodeEditable.Update.calculate(
          ~settings,
          ~autoprobe_mode,
          ~is_edited=false, /* Not an edit, just resolving pending focus/cursor */
          ~stitch,
          ~dynamics=EvalResult.Model.dynamics(result),
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
    |> Option.map(x => (Update.MainEditor(x), MainEditor));
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
    div(
      ~attrs=[Attr.classes(["cell", locked ? "locked" : "unlocked"])],
      Option.to_list(caption)
      @ [
        CodeEditable.View.view(
          ~globals,
          ~signal=
            locked
              ? _ => Ui_effect.Ignore
              : fun
                | MakeActive => signal(MakeActive(MainEditor)),
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
          ~dynamics=EvalResult.Model.dynamics(model.result),
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
