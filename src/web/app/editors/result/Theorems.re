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
    stepper_view: StepperView.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_theorem = {stepper_view: StepperView.Model.persistent};

  let theorem_init = name => {
    name,
    ctx: Calc.Pending,
    env: Calc.Pending,
    sem_ctx: Calc.Pending,
    goal_exp: Calc.Pending,
    stepper_view: StepperView.Model.init,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    thm_map: Id.Map.t(theorem),
    thms: Calc.saved(list(Id.t)),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {thm_map: Id.Map.t(persistent_theorem)};

  let init = {
    thm_map: Id.Map.empty,
    thms: Calc.Pending,
  };

  /* (proven, all), for the results' status line */
  let proof_count = (model: t): (int, int) => {
    let ids = Calc.get_saved([], model.thms);
    let proven =
      List.filter(
        id =>
          switch (Id.Map.find_opt(id, model.thm_map)) {
          | Some(th: theorem) =>
            StepperView.Model.get_validity(th.stepper_view) == Some(true)
          | None => false
          },
        ids,
      );
    (List.length(proven), List.length(ids));
  };

  let persist = (model: t): persistent => {
    thm_map:
      Id.Map.map(
        (thm: theorem) =>
          {stepper_view: StepperView.Model.persist(thm.stepper_view)},
        model.thm_map,
      ),
  };

  let unpersist = (p: persistent): t => {
    thm_map:
      Id.Map.map(
        (p_thm: persistent_theorem): theorem =>
          {
            name: "?",
            ctx: Calc.Pending,
            env: Calc.Pending,
            sem_ctx: Calc.Pending,
            goal_exp: Calc.Pending,
            stepper_view: StepperView.Model.unpersist(p_thm.stepper_view),
          },
        p.thm_map,
      ),
    thms: Calc.Pending,
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
              StepperView.Model.get_validity(thm.stepper_view) == Some(true)
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
    /* a proof drawer's cached view shows the old stepper until this moves */
    Haz3lcore.ProbeProj.Settings.version :=
      Haz3lcore.ProbeProj.Settings.version^ + 1;
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
        {thm_map, thms}: Model.t,
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
                   stepper_view,
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

                 let stepper_view =
                   StepperView.Update.calculate(
                     ~settings,
                     ~ctx=sem_ctx,
                     ~ana=Calc.OldValue(Typ.fresh(Atom(Bool))),
                     goal_exp,
                     stepper_view,
                   );

                 Some({
                   name,
                   ctx: ctx |> Calc.save,
                   env: env |> Calc.save,
                   sem_ctx: sem_ctx |> Calc.save,
                   goal_exp: goal_exp |> Calc.save,
                   stepper_view,
                 });
               },
               acc,
             ),
           thm_map,
         );

    Model.{
      thm_map,
      thms: thms |> Calc.save,
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

/* a proof drawer: its header row, then the proof's trace */
let rows = (~settings: CoreSettings.t, model: Model.t, id: Id.t): option(int) =>
  Id.Map.find_opt(id, model.thm_map)
  |> Option.map((thm: Model.theorem) =>
       1
       + ProbeSteps.stepper_rows(
           ~settings={
             ...settings,
             evaluation: {
               ...settings.evaluation,
               enable_proof: true,
               stepper_history: true,
             },
           },
           thm.stepper_view,
         )
     );

/* fit every theorem's proof drawer; true when one changed */
let fit = (~settings: CoreSettings.t, model: Model.t): bool =>
  Id.Map.fold(
    (id, _, changed) =>
      switch (rows(~settings, model, id)) {
      | Some(n) => Haz3lcore.ProofProj.Settings.set_computed(id, n) || changed
      | None => changed
      },
    model.thm_map,
    false,
  );

module View = {
  open WebUtil;

  let proof_globals = (globals: Globals.t): Globals.t => {
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

  /* one theorem's proof, for its drawer */
  let view_one =
      (
        ~globals: Globals.t,
        ~inject: Update.t => Ui_effect.t(unit),
        model: Model.t,
        id: Id.t,
      )
      : option(Node.t) => {
    let globals = proof_globals(globals);
    let thms = model.thms |> Calc.get_saved([]);
    switch (
      List.find_index(x => x == id, thms),
      Id.Map.find_opt(id, model.thm_map),
    ) {
    | (Some(idx), Some(thm)) =>
      let proven =
        StepperView.Model.get_validity(thm.stepper_view) == Some(true);
      Some(
        div_c(
          "theorem",
          [
            div_c(
              "theorem-header",
              [
                Node.text("Proof of " ++ thm.name),
                Node.div(
                  ~attrs=[
                    Attr.classes([
                      "theorem-status",
                      proven ? "true" : "unknown",
                    ]),
                  ],
                  [Node.text(proven ? "proven" : "incomplete")],
                ),
              ],
            ),
          ]
          @ StepperView.View.view(
              ~globals,
              ~signal=_ => Ui_effect.Ignore,
              ~inject=a => inject(Update.TheoremUpdate(idx, a)),
              ~selected=None,
              ~is_toplevel=false,
              thm.stepper_view,
            ),
        ),
      );
    | _ => None
    };
  };

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Focus.t => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        model: Model.t,
      ) => {
    let globals = proof_globals(globals);
    switch (model.thms |> Calc.get_saved([])) {
    | [] => []
    | xs =>
      List.mapi(
        (idx, id) => {
          let Model.{stepper_view, name, _} = Id.Map.find(id, model.thm_map);
          let status =
            switch (StepperView.Model.get_validity(stepper_view)) {
            | Some(true) =>
              Node.div(
                ~attrs=[Attr.classes(["theorem-status", "true"])],
                [Node.text("proven true")],
              )
            | Some(false)
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
              stepper_view,
            );
          div_c("theorem", [header, ...stepper]);
        },
        xs,
      )
    };
  };
};
