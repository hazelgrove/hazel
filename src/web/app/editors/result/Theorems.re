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

  let can_undo = (action: t) => {
    switch (action) {
    | TheoremUpdate(_, action) => StepperView.Update.can_undo(action)
    };
  };

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

    /* Lift the big-step ProofMap into a Calc-tracked value so changes to
     * the underlying dynamics propagate exactly once into each theorem's
     * stepper. Shared across all theorems in this cell. */
    let proof_map_calc =
      prev_proof_map
      |> {
        let.calc dynamics = dynamics;
        switch (dynamics) {
        | None => ProofMap.empty
        | Some(d: Dynamics.t) => d.proof_map
        };
      };
    let proof_map = proof_map_calc |> Calc.get_value;
    let info_map = (statics |> Calc.get_value).info_map;
    // Calculate visible steppers
    let thm_map =
      dynamics
      |> Calc.get_value
      |> (
        fun
        | None => []
        | Some(x) => x.theorems
      )
      |> List.map(((a, b, c, d)) => {
           let d' =
             ProofRule.exp_to_rule(
               d |> Substitution.in_exp(Environment.empty),
             );
           (a, b, c, d');
         })
      |> List.fold_left(
           (acc, (id, name, env', rule: ProofRule.t)) =>
             Id.Map.update(
               id,
               (opt: option(Model.theorem)) => {
                 let Model.{
                   name: _,
                   ctx,
                   env,
                   sem_ctx,
                   goal_exp,
                   proof,
                   stepper_view,
                   proof_mark,
                 } =
                   Option.value(~default=Model.theorem_init("?"), opt);

                 let goal_exp =
                   Calc.set(
                     ~eq=Exp.fast_equal_with_lexemes,
                     rule |> ProofRule.conclusion_exp,
                     goal_exp,
                   );

                 let ctx =
                   ctx
                   |> {
                     let.calc statics = statics;
                     statics.info_map
                     |> Statics.Map.ctx_of(id)
                     |> Option.value(~default=Ctx.empty)
                     |> List.fold_left(
                          Ctx.extend,
                          _,
                          rule.bindings |> List.rev,
                        );
                   };

                 let env = Calc.set(~eq=Environment.id_equal, env', env);

                 let sem_ctx =
                   sem_ctx
                   |> {
                     let.calc ctx = ctx
                     and.calc env = env;
                     SemanticCtx.of_ctx_and_env(ctx, env);
                   };

                 /* Lift the proof sub-term out of the Theorem syntax node.
                  * Calc.set keeps OldValue when the term is unchanged so
                  * the stepper only rebuilds when the proof actually
                  * changes. Shared with proof_mark's lookup below. */
                 let proof_lookup =
                   switch (Statics.Map.lookup_exp(id, info_map)) {
                   | Some({user_term, _}) =>
                     switch (user_term |> Exp.term_of) {
                     | Theorem(_, _, proof, _) => Some(proof)
                     | _ => None
                     }
                   | None => None
                   };
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
                     ~proof,
                     ~proof_map=proof_map_calc,
                     /* Whole-theorem statics, so the induction stepper's
                      * exhaustiveness label can read the static error on the
                      * scrutinee (kept in sync with the editor). */
                     ~proof_info_map=Calc.OldValue(info_map),
                     goal_exp,
                     stepper_view,
                   );

                 /* Derive the mark for the proof immediately inside this
                  * theorem by consulting the big-step ProofMap. */
                 let proof_mark = {
                   let mark =
                     switch (proof_lookup) {
                     | Some(p) => ProofMap.status_of_proof(proof_map, p)
                     | None => None
                     };
                   Calc.set(mark, proof_mark);
                 };

                 Some({
                   name,
                   ctx: ctx |> Calc.save,
                   env: env |> Calc.save,
                   sem_ctx: sem_ctx |> Calc.save,
                   goal_exp: goal_exp |> Calc.save,
                   proof: proof |> Calc.save,
                   stepper_view,
                   proof_mark: proof_mark |> Calc.save,
                 });
               },
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
    switch (model.thms |> Calc.get_saved([])) {
    | [] => []
    | xs =>
      List.mapi(
        (idx, id) => {
          let Model.{stepper_view, name, proof_mark, _} =
            Id.Map.find(id, model.thm_map);
          let status =
            switch (proof_mark |> Calc.get_saved_opt |> Option.join) {
            | Some(true) =>
              Node.div(
                ~attrs=[Attr.classes(["theorem-status", "true"])],
                [Node.text("proven true")],
              )
            | Some(false) =>
              Node.div(
                ~attrs=[Attr.classes(["theorem-status", "false"])],
                [Node.text("disproven")],
              )
            | None =>
              Node.div(
                ~attrs=[Attr.classes(["theorem-status", "unknown"])],
                [Node.text("incomplete")],
              )
            };
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
          div_c("theorem", [header, ...stepper]);
        },
        xs,
      )
    };
  };
};
