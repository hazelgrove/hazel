open Util;
open Language;

/* The theorems of a cell, each with its stepper and contracts. The
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
    /* The statement as written. The Add to statement exit rewrites one
     * of its `forall` binders, so it needs the syntactic term, ids and
     * all. */
    stmt: option(Exp.t),
    /* The proof as written; a hole when there is none. */
    proof: Proof.t,
    stepper: StepperView.Model.t,
    /* The obligations of the functions in the theorem's scope, discharged
     * at their definitions. */
    contracts: list(Obligation.t),
    /* The contracts shown open, by index into `contracts`. */
    open_contracts: list(int),
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
    | TheoremUpdate(Id.t, StepperView.Update.t)
    | ToggleContract(Id.t, int);

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
    | ToggleContract(id, i) =>
      switch (Id.Map.find_opt(id, model.thm_map)) {
      | Some(thm) =>
        let open_contracts =
          List.mem(i, thm.open_contracts)
            ? List.filter((!=)(i), thm.open_contracts)
            : [i, ...thm.open_contracts];
        {
          ...model,
          thm_map:
            Id.Map.add(
              id,
              {
                ...thm,
                open_contracts,
              },
              model.thm_map,
            ),
        }
        |> return_quiet(~logged=true);
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
      switch (Calc.get_value(checked)) {
      | None => Model.init
      | Some((checked_statics, dynamics)) =>
        /* The evaluator can't check proofs off the main thread, where it
         * has no statics, so each theorem the evaluation recorded is
         * checked here, with the env it recorded. Theorems are recorded
         * latest first, and a later theorem's entries win, as in the
         * evaluator. */
        let (proof_map, contracts_of) =
          if (Calc.is_new(checked)) {
            let checks =
              List.rev(dynamics.theorems)
              |> List.filter_map(((id, _, env, _)) =>
                   Option.bind(
                     Exp.find_by_id(id, checked_statics.elaborated), node =>
                     Evaluator.Proofs.check_theorem(
                       ~statics=checked_statics.info_map,
                       ~env,
                       node,
                     )
                   )
                   |> Option.map(checked => (id, checked))
                 );
            let proof_map =
              List.fold_left(
                (acc, (_, checked: Evaluator.Proofs.checked_theorem)) =>
                  ProofMap.union(acc, checked.proof_map),
                ProofMap.empty,
                checks,
              );
            let contracts_of = id =>
              switch (List.assoc_opt(id, checks)) {
              | Some({contracts, _}) =>
                Id.Map.fold(
                  (_, entry: ProofMap.entry, acc) => acc @ entry.obligations,
                  contracts,
                  [],
                )
              | None => []
              };
            (proof_map, contracts_of);
          } else {
            let contracts_of = id =>
              switch (Id.Map.find_opt(id, model.thm_map)) {
              | Some(thm) => thm.contracts
              | None => []
              };
            (model.proof_map, contracts_of);
          };
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
          let (stepper, open_contracts) =
            switch (Id.Map.find_opt(id, model.thm_map)) {
            | Some(thm) => (thm.stepper, thm.open_contracts)
            | None => (StepperView.Model.init, [])
            };
          let stepper =
            StepperView.Update.calculate(
              ~settings,
              ~checking,
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
              contracts: contracts_of(id),
              open_contracts,
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
      settings: {
        ...globals.settings,
        core: with_proof_settings(globals.settings.core),
      },
    };
    let proof_map = model.proof_map;
    let theorem =
        (
          (
            id,
            {name, stmt, proof, stepper, contracts, open_contracts}: Model.theorem,
          ),
        ) => {
      let status = ProofMap.full_status_of_proof(proof_map, proof);
      let counter =
        switch (Obligations.Status.open_count(proof_map, proof)) {
        | 0 => []
        | n => [
            Node.div(
              ~attrs=[
                Attr.classes(["theorem-open-count"]),
                Attr.title(
                  string_of_int(n)
                  ++ (n == 1 ? " open obligation" : " open obligations"),
                ),
              ],
              [Node.text(string_of_int(n) ++ " open")],
            ),
          ]
        };
      let toggle = i => inject(Update.ToggleContract(id, i));
      let contract_exits = (ob: Obligation.t) =>
        Obligations.Exits.for_contract(
          ~func=
            Statics.Map.lookup_exp(ob.origin, model.info_map)
            |> Option.map((info: Info.exp) => info.user_term),
          ob,
        );
      let dots =
        List.concat(
          List.mapi(
            (i, ob) =>
              List.mem(i, open_contracts)
                ? []
                : [StepperView.View.Condition.dot(~toggle=toggle(i), ob)],
            contracts,
          ),
        );
      let sections =
        List.concat(
          List.mapi(
            (i, ob) =>
              List.mem(i, open_contracts)
                ? [
                  StepperView.View.Condition.view(
                    ~globals,
                    ~edit_syntax,
                    ~exits=contract_exits(ob),
                    ~toggle=toggle(i),
                    ob,
                  ),
                ]
                : [],
            contracts,
          ),
        );
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
                  Obligations.Status.class_(status),
                ]),
              ],
              [Node.text(Obligations.Status.label(status))],
            ),
          ]
          @ counter
          @ (dots == [] ? [] : [div_c("theorem-contracts", dots)]),
        );
      let contracts =
        sections == [] ? [] : [div_c("theorem-contract-sections", sections)];
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
                stmt,
                proof,
                proof_map,
                info_map: model.info_map,
              },
            ),
          proof,
          stepper,
        );
      div_c("theorem", [header] @ contracts @ stepper);
    };
    List.map(theorem, Model.theorems(model));
  };
};
