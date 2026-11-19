open Util;
open Language;
open Poly;

/* The theorems of a cell, each with its stepper. The
 * theorems and their proof map come from the last completed evaluation,
 * checked with the statics of its request. Their proofs' syntax is read
 * from the current statics, so a step just written has its row (and its
 * sub-editors, which take focus) at once; it shows a goal once a check
 * reaches it. */

/* This file follows conventions in [docs/ui-architecture.md] */

/* Theorem steppers always show the proof's history. */
let with_proof_settings = (settings: CoreSettings.t): CoreSettings.t => {
  ...settings,
  evaluation: {
    ...settings.evaluation,
    enable_proof: true,
    stepper_history: true,
  },
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type theorem = {
    name: string,
    /* The proof as written; a hole when there is none. */
    proof: Proof.t,
    /* The program its proof was checked in. */
    program: StepperPicker.Program.t,
    stepper: StepperView.Model.t,
    /* While the program no longer reaches the theorem, or its proof can't
     * be found, the stepper is drawn as it last was, with the proof map
     * it was checked with, and keeps its state for when the theorem is
     * back. */
    stale: option(ProofMap.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    /* In program order. */
    thms: list(Id.t),
    thm_map: Id.Map.t(theorem),
    /* The cell's ProofMap, shared by all its theorems. */
    proof_map: ProofMap.t,
    /* The statics of the proofs' syntax. */
    info_map: Statics.Map.t,
  };

  let init = {
    thms: [],
    thm_map: Id.Map.empty,
    proof_map: ProofMap.empty,
    info_map: Statics.Map.empty,
  };

  let theorems = (model: t): list((Id.t, theorem)) =>
    List.filter_map(
      ~f=
        id =>
          Id.Map.find_opt(id, model.thm_map) |> Option.map(~f=t => (id, t)),
      model.thms,
    );

  let get_score = (model: t): option((float, float)) => {
    let thms =
      List.filter(~f=((_, thm)) => thm.stale == None, theorems(model));
    /* Only a proof with nothing unproven scores. One relying on an
     * unfinished lemma doesn't: the lemma could state the theorem itself. */
    let proven =
      List.filter(
        ~f=
          ((_, thm)) =>
            Obligations.Status.of_proof(model.proof_map, thm.proof) == Proven,
        thms,
      );
    Some((
      float_of_int(List.length(proven)),
      float_of_int(List.length(thms)),
    ));
  };
};

/* The theorems drawn last, by id. A theorem whose inputs are all the same
 * objects as when it was drawn gets the same node, which the virtual DOM
 * then skips: a key in one theorem's write box changes no other. Kept
 * outside the model, as only the view uses it. A kept node's handlers
 * close over these inputs and the cell's dispatchers alone, so they act
 * on the theorem as it is. */
module Drawn = {
  type inputs = {
    theorem: Model.theorem,
    proof_map: ProofMap.t,
    info_map: Statics.Map.t,
    selected: option(StepperView.Focus.t),
    main_editor: CodeEditable.Model.t,
    settings: Settings.t,
    font_metrics: FontMetrics.t,
    meta_down: bool,
    color_highlights: option(ColorSteps.colorMap),
  };

  let entries: Hashtbl.t(Id.t, (inputs, Virtual_dom.Vdom.Node.t)) =
    Hashtbl.create(16);

  let same = (a: inputs, b: inputs) =>
    phys_equal(a.theorem, b.theorem)
    && phys_equal(a.proof_map, b.proof_map)
    && phys_equal(a.info_map, b.info_map)
    && a.selected == b.selected
    /* The cell builds its editor's records again on every pass, its zipper
     * included, around the same syntax and statics. A sub-editor reads the
     * zipper only for its caret, so a theorem whose sub-editor has focus
     * is always drawn. */
    && (
      switch (b.selected) {
      | Some(Splice(_)) => false
      | Some(Goal(_) | Box(_))
      | None => true
      }
    )
    && phys_equal(a.main_editor.editor.syntax, b.main_editor.editor.syntax)
    && phys_equal(
         a.main_editor.statics.info_map,
         b.main_editor.statics.info_map,
       )
    && phys_equal(
         a.main_editor.statics.error_ids,
         b.main_editor.statics.error_ids,
       )
    && phys_equal(
         a.main_editor.statics.warning_ids,
         b.main_editor.statics.warning_ids,
       )
    && phys_equal(
         a.main_editor.statics.completion,
         b.main_editor.statics.completion,
       )
    && phys_equal(a.main_editor.dynamics, b.main_editor.dynamics)
    && a.main_editor.context_menu == b.main_editor.context_menu
    && phys_equal(a.settings, b.settings)
    && phys_equal(a.font_metrics, b.font_metrics)
    && a.meta_down == b.meta_down
    && phys_equal(a.color_highlights, b.color_highlights);

  let view = (id: Id.t, inputs: inputs, draw: unit => Virtual_dom.Vdom.Node.t) =>
    switch (Hashtbl.find_opt(entries, id)) {
    | Some((last, node)) when same(last, inputs) => node
    | Some(_)
    | None =>
      let node = draw();
      Hashtbl.replace(entries, id, (inputs, node));
      node;
    };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TheoremUpdate(Id.t, StepperView.Update.t);

  let update =
      (~settings: Settings.t, action, model: Model.t): Updated.t(Model.t) => {
    let settings = {
      ...settings,
      core: with_proof_settings(settings.core),
    };
    switch (action) {
    | TheoremUpdate(id, action) =>
      switch (Id.Map.find_opt(id, model.thm_map)) {
      | Some(thm) =>
        let* stepper =
          StepperView.Update.update(~settings, action, thm.stepper);
        {
          ...model,
          thm_map:
            Id.Map.add(
              id,
              {
                ...thm,
                stepper,
              },
              model.thm_map,
            ),
        };
      | None => model |> raise_invalid_action
      }
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        /* The cell's current statics. */
        ~statics: Calc.t(Haz3lcore.CachedStatics.t),
        /* A completed evaluation and the statics of its request
         * (EvalResult.Model.checked). */
        ~checked: Calc.t(option((Haz3lcore.CachedStatics.t, Dynamics.t))),
        /* Whether a newer evaluation is running, whose check will give the
         * goals of steps written since `checked`. */
        ~checking: bool,
        model: Model.t,
      )
      : Model.t =>
    switch (settings, statics, checked) {
    | (OldValue(_), OldValue(_), OldValue(_)) => model
    | _ =>
      let settings = with_proof_settings(Calc.get_value(settings));
      /* `after`, once the drawn nodes of the theorems it drops are
       * forgotten. */
      let prune = (after: Model.t): Model.t => {
        Id.Map.iter(
          (id, thm: Model.theorem) =>
            if (!Id.Map.mem(id, after.thm_map)) {
              Hashtbl.remove(Drawn.entries, id);
              StepperView.Drawn.prune(
                ~before=thm.stepper,
                ~after=StepperView.Model.init,
              );
            },
          model.thm_map,
        );
        after;
      };
      switch (Calc.get_value(checked)) {
      | None => prune(Model.init)
      | Some((checked_statics, dynamics)) =>
        let proof_map =
          Calc.is_new(checked) ? dynamics.proof_map : model.proof_map;
        /* The same object for as long as the check is the same, so a
         * stepper can tell its goals' offers still hold without comparing
         * them. */
        let program = (id): StepperPicker.Program.t =>
          switch (Id.Map.find_opt(id, model.thm_map)) {
          | Some({program, _})
              when phys_equal(program.info_map, checked_statics.info_map) => program
          | Some(_)
          | None => {
              info_map: checked_statics.info_map,
              typing:
                Statics.Map.lookup_exp(id, checked_statics.info_map)
                |> Option.map(~f=(info: Info.exp) => info.ctx)
                |> Option.value(~default=Ctx.empty),
            }
          };
        let info_map = Calc.get_value(statics).info_map;
        let syntax = id =>
          switch (Statics.Map.lookup_exp(id, info_map)) {
          | Some(info) => Some(info)
          | None => Statics.Map.lookup_exp(id, checked_statics.info_map)
          };
        let stale = (thm: Model.theorem) => {
          ...thm,
          stale: Some(Option.value(~default=model.proof_map, thm.stale)),
        };
        /* Each theorem keeps its stepper's state from the last pass. */
        let theorem = ((id, name, _, _)) =>
          switch (
            syntax(id)
            |> Option.map(~f=(info: Info.exp) => Exp.term_of(info.user_term)),
            Id.Map.find_opt(id, model.thm_map),
          ) {
          | (Some(Theorem(_, _, proof, _)), last) =>
            let stepper =
              switch (last) {
              | Some(thm) => thm.stepper
              | None => StepperView.Model.init
              };
            let program = program(id);
            let stepper =
              StepperView.Update.calculate(
                ~settings,
                ~program,
                ~checking,
                StepperRows.of_theorem(~proof_map, proof),
                stepper,
              );
            (
              id,
              Model.{
                name,
                proof,
                program,
                stepper,
                stale: None,
              },
            );
          | (_, Some(thm)) => (id, stale(thm))
          | (_, None) => (
              id,
              Model.{
                name,
                proof: Proof.fresh(EmptyHole),
                program: program(id),
                stepper: StepperView.Model.init,
                stale: None,
              },
            )
          };
        let reached = List.rev_map(~f=theorem, dynamics.theorems);
        /* A theorem the evaluation didn't reach, after a typo above it,
         * stays while it is still in the program. */
        let unreached =
          List.filter_map(
            ~f=
              id =>
                switch (
                  List.Assoc.mem(reached, id, ~equal=Id.equal),
                  Id.Map.find_opt(id, model.thm_map),
                  Statics.Map.lookup_exp(id, info_map),
                ) {
                | (false, Some(thm), Some(_)) => Some((id, stale(thm)))
                | (true, _, _)
                | (false, None, _)
                | (false, _, None) => None
                },
            model.thms,
          );
        let theorems = reached @ unreached;
        prune({
          thms: List.map(~f=fst, theorems),
          thm_map: Id.Map.of_list(theorems),
          proof_map,
          info_map,
        });
      };
    };
};

module Focus = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Id.t, StepperView.Focus.t);

  let get_cursor_info = (~inject, ~focus as (id, focus): t, model: Model.t) =>
    switch (Id.Map.find_opt(id, model.thm_map)) {
    | Some(thm) =>
      let+ c =
        StepperView.Focus.get_cursor_info(
          ~inject=x => inject(Update.TheoremUpdate(id, x)),
          ~focus,
          thm.stepper,
        );
      Update.TheoremUpdate(id, c);
    | None => Cursor.empty
    };
};

module View = {
  open WebUtil;

  /* The steppers' settings, the same object while they are the same, so
   * that a stepper's rows can tell that nothing they show changed. The
   * cell builds its settings again on every render, so they are compared
   * by value. */
  let last_settings: ref(option(Settings.t)) = ref(None);
  let stepper_settings = (settings: Settings.t): Settings.t => {
    let steppers = {
      ...settings,
      core: with_proof_settings(settings.core),
    };
    switch (last_settings^) {
    | Some(last) when last == steppers => last
    | Some(_)
    | None =>
      last_settings := Some(steppers);
      steppers;
    };
  };

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Focus.t => Ui_effect.t(unit),
        ~return_to_editor: Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        /* Writes a patch into the cell's program. */
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        /* The cell's editor, which the steppers' sub-editors splice into. */
        ~main_editor: CodeEditable.Channel.t,
        model: Model.t,
      ) => {
    let globals = {
      ...globals,
      settings: stepper_settings(globals.settings),
    };
    let theorem =
        ((id, {name, proof, program, stepper, stale}: Model.theorem)) => {
      let proof_map = Option.value(~default=model.proof_map, stale);
      /* A stale stepper is for reading: its proof may not be where it was
       * in the program. */
      let edit_syntax = stale == None ? edit_syntax : (_ => Ui_effect.Ignore);
      let status = Obligations.Status.of_proof(proof_map, proof);
      let dom_id = "theorem-" ++ Id.to_string(id);
      let header =
        div_c(
          "theorem-header",
          [
            Node.strong([Node.text("Proof of theorem ")]),
            Node.text(name),
            switch (stale) {
            | Some(_) =>
              Node.div(
                ~attrs=[
                  Attr.classes(["theorem-status", "stale"]),
                  Attr.title(
                    "The program doesn't run as far as this theorem, or its proof can't be found. This is how it was last checked.",
                  ),
                ],
                [Node.text("not checked")],
              )
            | None =>
              let to_first_gap = _ =>
                Ui_effect.of_sync_fun(
                  () =>
                    Js_of_ocaml.(
                      Dom_html.document##querySelector(
                        Js.string(
                          "#"
                          ++ dom_id
                          ++ " .condition-dot.unproven:not(.citation)",
                        ),
                      )
                    )
                    |> Js_of_ocaml.Js.Opt.to_option
                    |> Option.iter(
                         ~f=JsUtil.scroll_vertically_into_view_ancestors,
                       ),
                  (),
                );
              Node.div(
                ~attrs=
                  [
                    Attr.classes([
                      "theorem-status",
                      Obligations.Status.class_(status),
                    ]),
                  ]
                  @ (
                    switch (status) {
                    | Relying(names) => [
                        Attr.title(
                          "Proven, assuming "
                          ++ String.concat(~sep=", ", names)
                          ++ ": cited here, but not proven yet.",
                        ),
                      ]
                    | Incomplete(n) when n > 0 => [
                        Attr.title(
                          "Not proven: "
                          ++ (n == 1 ? "a condition is" : "conditions are")
                          ++ " still open. Click to find the first; its circle opens it.",
                        ),
                        Attr.on_click(to_first_gap),
                      ]
                    | Proven
                    | Refuted
                    | Incomplete(_) => []
                    }
                  ),
                [Node.text(Obligations.Status.label(status))],
              );
            },
          ],
        );
      let stepper =
        StepperView.View.view(
          ~globals,
          ~signal=
            fun
            | MakeActive(f) => take_focus((id, f))
            | HideStepper => Ui_effect.Ignore
            | ReturnToEditor => return_to_editor,
          ~inject=a => inject(Update.TheoremUpdate(id, a)),
          ~selected=
            switch (selected) {
            | Some((id', f)) when id == id' => Some(f)
            | _ => None
            },
          ~is_toplevel=false,
          ~edit_syntax,
          ~backing=main_editor,
          ~info_map=model.info_map,
          ~proof_map,
          ~exits=
            Obligations.Exits.for_step(
              ~ctx={
                proof,
                proof_map,
                program,
              },
            ),
          proof,
          stepper,
        );
      Node.div(
        ~attrs=[
          Attr.id(dom_id),
          Attr.classes(["theorem"] @ (stale == None ? [] : ["stale"])),
        ],
        [header] @ stepper,
      );
    };
    List.map(
      ~f=
        ((id, thm: Model.theorem)) =>
          Drawn.view(
            id,
            {
              theorem: thm,
              proof_map: model.proof_map,
              info_map: model.info_map,
              selected:
                switch (selected) {
                | Some((id', f)) when id == id' => Some(f)
                | _ => None
                },
              main_editor: main_editor.model,
              settings: globals.settings,
              font_metrics: globals.font_metrics,
              meta_down: globals.meta_down,
              color_highlights: globals.color_highlights,
            },
            () =>
            theorem((id, thm))
          ),
      Model.theorems(model),
    );
  };
};
