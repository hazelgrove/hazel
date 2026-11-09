open Util;
open Language;

/* The theorems of a cell, each with its stepper and obligations. The
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
    /* The statement as written. The obligations panel's float-to-binder
     * action rewrites one of its `forall` binders, so it needs the
     * syntactic term, ids and all. */
    stmt: option(Exp.t),
    /* The proof as written; a hole when there is none. */
    proof: Proof.t,
    stepper: StepperView.Model.t,
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
      id => Id.Map.find_opt(id, model.thm_map) |> Option.map(t => (id, t)),
      model.thms,
    );

  let get_score = (model: t): option((float, float)) => {
    let thms = theorems(model);
    let proven =
      List.filter(
        ((_, thm)) =>
          ProofMap.status_of_proof(model.proof_map, thm.proof) == Some(true),
        thms,
      );
    Some((
      float_of_int(List.length(proven)),
      float_of_int(List.length(thms)),
    ));
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
        model: Model.t,
      )
      : Model.t =>
    switch (settings, statics, checked) {
    | (OldValue(_), OldValue(_), OldValue(_)) => model
    | _ =>
      let settings = with_proof_settings(Calc.get_value(settings));
      switch (Calc.get_value(checked)) {
      | None => Model.init
      | Some((checked_statics, dynamics)) =>
        /* The evaluator can't check proofs off the main thread, where it
         * has no statics, so each theorem the evaluation recorded is
         * checked here, with the env it recorded. Theorems are recorded
         * latest first, and a later theorem's entries win, as in the
         * evaluator. */
        let proof_map =
          Calc.is_new(checked)
            ? List.fold_left(
                (acc, (id, _, env, _)) =>
                  switch (
                    Option.bind(
                      Exp.find_by_id(id, checked_statics.elaborated), node =>
                      Evaluator.Proofs.check_theorem(
                        ~statics=checked_statics.info_map,
                        ~env,
                        node,
                      )
                    )
                  ) {
                  | Some(pm) => ProofMap.union(acc, pm)
                  | None => acc
                  },
                ProofMap.empty,
                List.rev(dynamics.theorems),
              )
            : model.proof_map;
        let info_map = Calc.get_value(statics).info_map;
        let syntax = id =>
          switch (Statics.Map.lookup_exp(id, info_map)) {
          | Some(info) => Some(info)
          | None => Statics.Map.lookup_exp(id, checked_statics.info_map)
          };
        /* Each theorem keeps its stepper's state from the last pass, and a
         * theorem the evaluation didn't reach goes with its state. */
        let theorem = ((id, name, _, _)) => {
          let (stmt, proof) =
            switch (
              syntax(id)
              |> Option.map((info: Info.exp) => Exp.term_of(info.user_term))
            ) {
            | Some(Theorem(_, stmt, proof, _)) => (Some(stmt), proof)
            | _ => (None, Proof.fresh(EmptyHole))
            };
          let stepper =
            switch (Id.Map.find_opt(id, model.thm_map)) {
            | Some(thm) => thm.stepper
            | None => StepperView.Model.init
            };
          let stepper =
            StepperView.Update.calculate(
              ~settings,
              StepperRows.chain(~proof_map, proof),
              stepper,
            );
          (
            id,
            Model.{
              name,
              stmt,
              proof,
              stepper,
            },
          );
        };
        let theorems = List.rev_map(theorem, dynamics.theorems);
        {
          thms: List.map(fst, theorems),
          thm_map: Id.Map.of_list(theorems),
          proof_map,
          info_map,
        };
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

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Focus.t => Ui_effect.t(unit),
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
      settings: {
        ...globals.settings,
        core: with_proof_settings(globals.settings.core),
      },
    };
    let proof_map = model.proof_map;
    switch (Model.theorems(model)) {
    | [] => []
    | theorems =>
      /* Definition-time obligations belong to the definitions the cell's
       * theorems can see, not to any one theorem, so they are rendered
       * once at the end (see ObligationsPanel.view_definitions). Every
       * theorem's proof is excluded from the walk. */
      let definitions =
        ObligationsPanel.view_definitions(
          ~globals,
          ObligationsPanel.group_of(
            ~pm=proof_map,
            ~proofs=
              List.map(((_, thm: Model.theorem)) => thm.proof, theorems),
          ),
        );
      let theorem = ((id, {name, stmt, proof, stepper}: Model.theorem)) => {
        /* The obligation-aware status, so ProvenModulo reads distinctly
         * from both proven and incomplete. */
        let full_status = ProofMap.full_status_of_proof(proof_map, proof);
        let header =
          div_c(
            "theorem-header",
            [
              Node.strong([Node.text("Proof of theorem ")]),
              Node.text(name),
              Node.div(
                ~attrs=[
                  Attr.classes([
                    "theorem-status",
                    ObligationsPanel.status_class(full_status),
                  ]),
                ],
                [Node.text(ObligationsPanel.status_label(full_status))],
              ),
            ],
          );
        let stepper =
          StepperView.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive(f) => take_focus((id, f))
              | HideStepper => Ui_effect.Ignore,
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
            proof,
            stepper,
          );
        /* This theorem's own obligations, with their receipts and the
         * three-exit action menu on each pending row. The float action
         * rewrites a binder of the statement, the wrapping actions a
         * region of the proof. */
        let obligations =
          ObligationsPanel.view(
            ~globals,
            ~edit_syntax,
            ~action_ctx={
              stmt,
              proof: Some(proof),
            },
            ~main_editor=Some(main_editor),
            ObligationsPanel.group_of(~pm=proof_map, ~proofs=[proof]),
          );
        div_c("theorem", [header, ...stepper] @ obligations);
      };
      List.map(theorem, theorems) @ definitions;
    };
  };
};
