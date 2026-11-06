open Util;
open Calc.Syntax;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type theorem = {
    name: string,
    ctx: Calc.saved(Ctx.t),
    env: Calc.saved(Environment.t(Exp.t)),
    sem_ctx: Calc.saved(SemanticCtx.t),
    goal_exp: Calc.saved(Exp.t),
    proof: Calc.saved(option(Proof.t)),
    stepper_view: StepperView.Model.t,
    /* Mark derived from the big-step ProofMap for the proof term directly
     * inside this theorem: Some(true) = proven, Some(false) = disproven
     * (outgoing is literally false), None = incomplete / not yet proven
     * (holes, failed steps, or no map entry). */
    proof_mark: Calc.saved(option(bool)),
    /* The obligation-aware status (ProofMap.full_status_of_proof), which
     * additionally distinguishes ProvenModulo — the goal reached `true`
     * but pending obligations remain.
     * `get_score` grades on the bool `proof_mark` above instead. */
    full_status: Calc.saved(ProofMap.full_status),
    /* The theorem's statement as written, lifted from the same syntax
     * node as `proof`. The obligations panel's float-to-binder action
     * rewrites one of its `forall` binders, so it needs the syntactic
     * term (ids and all), not the substituted one from dynamics. */
    stmt: Calc.saved(option(Exp.t)),
  };

  let theorem_init = name => {
    name,
    ctx: Calc.Pending,
    env: Calc.Pending,
    sem_ctx: Calc.Pending,
    goal_exp: Calc.Pending,
    proof: Calc.Pending,
    stepper_view: StepperView.Model.init,
    proof_mark: Calc.Pending,
    full_status: Calc.Pending,
    stmt: Calc.Pending,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    thm_map: Id.Map.t(theorem),
    thms: Calc.saved(list(Id.t)),
    proof_map: Calc.saved(ProofMap.t),
  };

  let init = {
    thm_map: Id.Map.empty,
    thms: Calc.Pending,
    proof_map: Calc.Pending,
  };

  let get_score = (model: t): option((float, float)) => {
    open OptUtil.Syntax;
    let* thms = model.thms |> Calc.get_saved_opt;
    let total = float_of_int(List.length(thms));
    let correct =
      List.fold_left(
        (acc, id) =>
          acc
          +. (
            switch (Id.Map.find_opt(id, model.thm_map)) {
            | Some(thm) =>
              Calc.get_saved_opt(thm.proof_mark) |> Option.join == Some(true)
                ? 1.0 : 0.0
            | None => 0.0
            }
          ),
        0.0,
        thms,
      );
    Some((correct, total));
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TheoremUpdate(int, StepperView.Update.t);

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    let settings =
      Settings.Model.{
        ...settings,
        core: {
          ...settings.core,
          evaluation: {
            ...settings.core.evaluation,
            enable_proof: true,
            stepper_history: true,
          },
        },
      };
    switch (action) {
    | TheoremUpdate(n, action) =>
      let id_and_thm = {
        open OptUtil.Syntax;
        let* id = List.nth_opt(model.thms |> Calc.get_saved([]), n);
        let* thm = Id.Map.find_opt(id, model.thm_map);
        Some((id, thm));
      };
      switch (id_and_thm) {
      | Some((id, thm)) =>
        let* stepper_view =
          StepperView.Update.update(~settings, action, thm.stepper_view);
        let thm_map =
          Id.Map.add(
            id,
            {
              ...thm,
              stepper_view,
            },
            model.thm_map,
          );
        Model.{
          ...model,
          thm_map,
        };
      | None => model |> Updated.raise_invalid_action
      };
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~statics: Calc.t(Haz3lcore.CachedStatics.t),
        ~dynamics: Calc.t(option(Dynamics.t)),
        /* A completed evaluation and the statics of its request, which the
         * proof map is checked with (EvalResult.Model.checked). */
        ~checked: Calc.t(option((Haz3lcore.CachedStatics.t, Dynamics.t))),
        {thm_map, thms, proof_map: prev_proof_map}: Model.t,
      ) => {
    let settings' = {
      ...Calc.get_value(settings),
      evaluation: {
        ...Calc.get_value(settings).evaluation,
        enable_proof: true,
        stepper_history: true,
      },
    };
    let settings =
      switch (settings) {
      | OldValue(_) => Calc.OldValue(settings')
      | NewValue(_) => Calc.NewValue(settings')
      };
    let thms =
      thms
      |> {
        let.calc dynamics = dynamics;
        let theorems =
          switch (dynamics) {
          | None => []
          | Some(d) => d.theorems
          };
        let theorems =
          List.map(
            ((a, b, c, d)) => {
              let d' = ProofRule.exp_to_rule(d);
              (a, b, c, d');
            },
            theorems,
          );
        List.map(((id, _, _, _)) => id, theorems) |> List.rev;
      }
      |> Calc.old_if_same'(thms);

    /* The cell's ProofMap, shared by all its theorems' steppers. The
     * evaluator can't check proofs off the main thread, where it has no
     * statics, so each theorem the evaluation recorded is checked here,
     * with the env it recorded. Theorems are recorded latest first, and a
     * later theorem's entries win, as in the evaluator. */
    let proof_map_calc =
      prev_proof_map
      |> {
        let.calc checked = checked;
        switch (checked) {
        | None => ProofMap.empty
        | Some((statics: Haz3lcore.CachedStatics.t, d: Dynamics.t)) =>
          List.fold_left(
            (acc, (id, _, env, _)) =>
              switch (Exp.find_by_id(id, statics.elaborated)) {
              | Some(node) =>
                switch (
                  Evaluator.Proofs.check_theorem(
                    ~statics=statics.info_map,
                    ~env,
                    node,
                  )
                ) {
                | Some(pm) => ProofMap.union(acc, pm)
                | None => acc
                }
              | None => acc
              },
            ProofMap.empty,
            List.rev(d.theorems),
          )
        };
      };
    let proof_map = proof_map_calc |> Calc.get_value;
    let info_map = (statics |> Calc.get_value).info_map;
    /* One theorem's stepper and status, from what the evaluation recorded
     * for it: its id, name, environment and statement. */
    let calculate_theorem =
        (
          (id, name, env', stmt: Exp.t),
          {
            name: _,
            ctx,
            env,
            sem_ctx,
            goal_exp,
            proof,
            stepper_view,
            proof_mark,
            full_status,
            stmt: stmt_saved,
          }: Model.theorem,
        )
        : Model.theorem => {
      /* The stepper's goal is the statement's core, after the outer
       * binders the checker introduces too (ProofCheck.peel_stmt_binders):
       * `==>` antecedents stay in the goal, and `where` restrictions
       * become hypotheses in `sem_ctx`. */
      let goal_exp =
        Calc.set(
          ~eq=Exp.fast_equal_with_lexemes,
          stmt |> ProofRule.peel_binders |> (((_, _, core)) => core),
          goal_exp,
        );
      let ctx =
        ctx
        |> {
          let.calc statics = statics;
          statics.info_map
          |> Statics.Map.ctx_of(id)
          |> Option.value(~default=Ctx.empty);
        };
      let env = Calc.set(~eq=Environment.id_equal, env', env);
      let sem_ctx =
        sem_ctx
        |> {
          let.calc ctx = ctx
          and.calc env = env;
          ProofCheck.peel_stmt_binders(
            SemanticCtx.of_program_state(ctx, env),
            stmt,
          )
          |> fst;
        };
      /* The statement and proof as written, from the Theorem syntax node.
       * Calc.set keeps OldValue while they are unchanged, so the stepper
       * rebuilds only when the proof changes. */
      let theorem_syntax =
        switch (Statics.Map.lookup_exp(id, info_map)) {
        | Some({user_term, _}) =>
          switch (user_term |> Exp.term_of) {
          | Theorem(_, stmt, proof, _) => Some((stmt, proof))
          | _ => None
          }
        | None => None
        };
      let proof_lookup = Option.map(snd, theorem_syntax);
      let stmt_saved =
        Calc.set(
          ~eq=
            (a, b) =>
              switch (a, b) {
              | (Some(a), Some(b)) => Exp.fast_equal(a, b)
              | (None, None) => true
              | _ => false
              },
          Option.map(fst, theorem_syntax),
          stmt_saved,
        );
      let proof =
        Calc.set(
          ~eq=
            (a, b) =>
              switch (a, b) {
              | (Some(a), Some(b)) => Proof.fast_equal(a, b)
              | (None, None) => true
              | _ => false
              },
          proof_lookup,
          proof,
        );
      let stepper_view =
        StepperView.Update.calculate(
          ~settings,
          ~ctx=sem_ctx,
          ~ana=Calc.OldValue(Typ.fresh(Atom(Bool))),
          /* With no proof written, the stepper shows a hole. */
          ~proof=
            Calc.map(Option.value(~default=Proof.fresh(EmptyHole)), proof),
          ~proof_map=proof_map_calc,
          /* The whole theorem's statics, where the induction row reads
           * whether its cases are exhaustive. */
          ~proof_info_map=Calc.OldValue(info_map),
          goal_exp,
          stepper_view,
        );
      let proof_mark =
        Calc.set(
          switch (proof_lookup) {
          | Some(p) => ProofMap.status_of_proof(proof_map, p)
          | None => None
          },
          proof_mark,
        );
      /* Like `proof_mark`, but a `true` with obligations still pending in
       * the proof is ProvenModulo. */
      let full_status =
        Calc.set(
          switch (proof_lookup) {
          | Some(p) => ProofMap.full_status_of_proof(proof_map, p)
          | None => ProofMap.Incomplete
          },
          full_status,
        );
      {
        name,
        ctx: ctx |> Calc.save,
        env: env |> Calc.save,
        sem_ctx: sem_ctx |> Calc.save,
        goal_exp: goal_exp |> Calc.save,
        proof: proof |> Calc.save,
        stepper_view,
        proof_mark: proof_mark |> Calc.save,
        full_status: full_status |> Calc.save,
        stmt: stmt_saved |> Calc.save,
      };
    };
    let thm_map =
      (
        switch (Calc.get_value(dynamics)) {
        | None => []
        | Some(d) => d.theorems
        }
      )
      |> List.fold_left(
           (acc, (id, name, env, stmt)) =>
             Id.Map.update(
               id,
               prev =>
                 Some(
                   calculate_theorem(
                     (
                       id,
                       name,
                       env,
                       Substitution.in_exp(Environment.empty, stmt),
                     ),
                     Option.value(~default=Model.theorem_init("?"), prev),
                   ),
                 ),
               acc,
             ),
           thm_map,
         );

    Model.{
      thm_map,
      thms: thms |> Calc.save,
      proof_map: proof_map_calc |> Calc.save,
    };
  };
};

module Focus = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (int, StepperView.Focus.t);

  let get_cursor_info = (~inject, ~focus: t, model: Model.t) => {
    let id_and_thm = {
      open OptUtil.Syntax;
      let* id = List.nth_opt(model.thms |> Calc.get_saved([]), focus |> fst);
      let* thm = Id.Map.find_opt(id, model.thm_map);
      Some((id, thm));
    };
    switch (id_and_thm) {
    | Some((_id, thm)) =>
      let+ c =
        StepperView.Focus.get_cursor_info(
          ~inject=x => inject(Update.TheoremUpdate(focus |> fst, x)),
          ~focus=snd(focus),
          thm.stepper_view,
        );
      Update.TheoremUpdate(focus |> fst, c);
    | None => Cursor.empty
    };
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
        /* Side-channel: when a proof-step view publishes a syntactic
         * edit, this callback routes the patch up to the host that owns
         * the main editor (typically CellEditor, which translates it
         * into an `PatchMainEditor` action). Defaults to no-op for
         * standalone use; only the cell-level caller wires a real
         * receiver. */
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
        /* Main-editor capability handle for sub-editor step views (see
         * SubEditor.re). Forwarded to StepperView. */
        ~main_editor: option(CodeEditable.Channel.t)=None,
        model: Model.t,
      ) => {
    let globals = {
      ...globals,
      settings: {
        ...globals.settings,
        core: {
          ...globals.settings.core,
          evaluation: {
            ...globals.settings.core.evaluation,
            enable_proof: true,
            stepper_history: true,
          },
        },
      },
    };
    let proof_map = model.proof_map |> Calc.get_saved(ProofMap.empty);
    switch (model.thms |> Calc.get_saved([])) {
    | [] => []
    | xs =>
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
              xs
              |> List.filter_map(id =>
                   Id.Map.find_opt(id, model.thm_map)
                   |> Option.map((t: Model.theorem) => t.proof)
                   |> Option.map(Calc.get_saved_opt)
                   |> Option.join
                   |> Option.join
                 ),
          ),
        );
      List.mapi(
        (idx, id) => {
          let Model.{stepper_view, name, full_status, proof, stmt, _} =
            Id.Map.find(id, model.thm_map);
          /* The obligation-aware status, so ProvenModulo reads distinctly
           * from both proven and incomplete. */
          let full_status =
            full_status
            |> Calc.get_saved_opt
            |> Option.value(~default=ProofMap.Incomplete);
          let status =
            Node.div(
              ~attrs=[
                Attr.classes([
                  "theorem-status",
                  ObligationsPanel.status_class(full_status),
                ]),
              ],
              [Node.text(ObligationsPanel.status_label(full_status))],
            );
          /* This theorem's own obligations, with their receipts and the
           * three-exit action menu on each pending row. The action context
           * is this theorem's own statement and proof syntax: the float
           * action rewrites a binder of the former, the wrapping actions a
           * region of the latter. Patches are routed out through
           * `edit_syntax`, the same channel the proof-step views use. */
          let this_proof = proof |> Calc.get_saved_opt |> Option.join;
          let action_ctx =
            ObligationsPanel.{
              stmt: stmt |> Calc.get_saved_opt |> Option.join,
              proof: this_proof,
            };
          let obligations =
            ObligationsPanel.view(
              ~globals,
              ~edit_syntax,
              ~action_ctx,
              ~main_editor,
              ObligationsPanel.group_of(
                ~pm=proof_map,
                ~proofs=this_proof |> Option.to_list,
              ),
            );
          let header =
            WebUtil.div_c(
              "theorem-header",
              [
                Node.strong([Node.text("Proof of theorem ")]),
                Node.text(name),
                status,
              ],
            );
          let stepper =
            StepperView.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive(f) => take_focus((idx, f))
                | HideStepper => Ui_effect.Ignore,
              ~inject=a => inject(Update.TheoremUpdate(idx, a)),
              ~selected=
                switch (selected) {
                | Some((idx', s)) when idx == idx' => Some(s)
                | _ => None
                },
              ~is_toplevel=false,
              ~edit_syntax,
              ~main_editor,
              stepper_view,
            );
          div_c("theorem", [header, ...stepper] @ obligations);
        },
        xs,
      )
      @ definitions;
    };
  };
};
