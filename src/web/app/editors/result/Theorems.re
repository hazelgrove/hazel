open Util;
open Calc.Syntax;
open Language;

let env_with_symbolic_ctx_vars =
    (ctx: Ctx.t, env: Environment.t(Exp.t)): Environment.t(Exp.t) =>
  Ctx.get_var_entries(ctx)
  |> List.fold_left(
       (env, var_entry: Ctx.var_entry) =>
         switch (Environment.lookup(env, var_entry.name)) {
         | Some(_) => env
         | None =>
           Environment.extend(
             ~id=var_entry.id,
             env,
             (var_entry.name, Exp.fresh(Var(var_entry.name))),
           )
         },
       env,
     );

let proof_core_settings = (settings: CoreSettings.t): CoreSettings.t => {
  ...settings,
  evaluation: {
    ...settings.evaluation,
    enable_proof: true,
    stepper_history: true,
  },
};

let proof_settings = (settings: Settings.Model.t): Settings.Model.t =>
  Settings.Model.{
    ...settings,
    core: proof_core_settings(settings.core),
  };

let proof_calc_settings = (settings: Calc.t(CoreSettings.t)) => {
  let settings' = proof_core_settings(Calc.get_value(settings));
  switch (settings) {
  | OldValue(_) => Calc.OldValue(settings')
  | NewValue(_) => Calc.NewValue(settings')
  };
};

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
    explore_map: Id.Map.t(theorem),
    explores: Calc.saved(list(Id.t)),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {thm_map: Id.Map.t(persistent_theorem)};

  let init = {
    thm_map: Id.Map.empty,
    thms: Calc.Pending,
    explore_map: Id.Map.empty,
    explores: Calc.Pending,
  };

  let persist = (model: t): persistent => {
    thm_map:
      Id.Map.map(
        (thm: theorem): persistent_theorem =>
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
    explore_map: Id.Map.empty,
    explores: Calc.Pending,
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
    | TheoremUpdate(int, StepperView.Update.t)
    | ExploreUpdate(int, StepperView.Update.t);

  let can_undo = (action: t) => {
    switch (action) {
    | TheoremUpdate(_, action) => StepperView.Update.can_undo(action)
    | ExploreUpdate(_, action) => StepperView.Update.can_undo(action)
    };
  };

  let update_stepper =
      (~settings, ~action, theorem: Model.theorem): Updated.t(Model.theorem) => {
    let* stepper_view =
      StepperView.Update.update(~settings, action, theorem.stepper_view);
    {
      ...theorem,
      stepper_view,
    };
  };

  let calculate_stepper =
      (
        ~settings,
        ~sem_ctx,
        ~ana: option(Calc.t(Typ.t)),
        ~goal_exp,
        stepper_view,
      )
      : StepperView.Model.t =>
    switch (ana) {
    | Some(ana) =>
      StepperView.Update.calculate(
        ~settings,
        ~ctx=sem_ctx,
        ~ana,
        goal_exp,
        stepper_view,
      )
    | None =>
      StepperView.Update.calculate(
        ~settings,
        ~ctx=sem_ctx,
        goal_exp,
        stepper_view,
      )
    };

  let has_changed_goal = (previous: Calc.saved(Exp.t), current: Exp.t): bool =>
    switch (previous |> Calc.get_saved_opt) {
    | Some(previous) => !Equality.ignoring_ascriptions.exp(previous, current)
    | None => false
    };

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    let settings = proof_settings(settings);
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
        let* thm = update_stepper(~settings, ~action, thm);
        let thm_map = Id.Map.add(id, thm, model.thm_map);
        Model.{
          ...model,
          thm_map,
        };
      | None => model |> Updated.raise_invalid_action
      };
    | ExploreUpdate(n, action) =>
      let id_and_explore = {
        open OptUtil.Syntax;
        let* id = List.nth_opt(model.explores |> Calc.get_saved([]), n);
        let* explore = Id.Map.find_opt(id, model.explore_map);
        Some((id, explore));
      };
      switch (id_and_explore) {
      | Some((id, explore)) =>
        let* explore = update_stepper(~settings, ~action, explore);
        let explore_map = Id.Map.add(id, explore, model.explore_map);
        Model.{
          ...model,
          explore_map,
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
        {thm_map, thms, explore_map, explores}: Model.t,
      ) => {
    let stepper_settings = proof_calc_settings(settings);
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
    let thm_ids = Calc.get_value(thms);
    let previous_thm_map = thm_map;
    let thm_map = Id.Map.filter((id, _) => List.mem(id, thm_ids), thm_map);

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
                 let conclusion_exp = rule |> ProofRule.conclusion_exp;
                 let carried_stepper =
                   Id.Map.fold(
                     (_, thm: Model.theorem, acc) =>
                       switch (acc, thm.goal_exp |> Calc.get_saved_opt) {
                       | (Some(_), _) => acc
                       | (None, Some(goal_exp))
                           when
                             thm.name == name
                             && Equality.ignoring_ascriptions.exp(
                                  goal_exp,
                                  conclusion_exp,
                                ) =>
                         Some(StepperView.Model.persist(thm.stepper_view))
                       | _ => None
                       },
                     previous_thm_map,
                     None,
                   );
                 let seeded_theorem = stepper =>
                   Model.{
                     ...theorem_init("?"),
                     stepper_view: StepperView.Model.unpersist(stepper),
                   };
                 let Model.{
                   name: _,
                   ctx,
                   env,
                   sem_ctx,
                   goal_exp,
                   stepper_view,
                 } =
                   switch (opt) {
                   | Some(thm) => thm
                   | None =>
                     switch (carried_stepper) {
                     | Some(stepper) => seeded_theorem(stepper)
                     | None => Model.theorem_init("?")
                     }
                   };

                 let goal_changed =
                   has_changed_goal(goal_exp, conclusion_exp);

                 let goal_exp =
                   Calc.set(~eq=Exp.fast_equal, conclusion_exp, goal_exp);

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
                   goal_changed ? StepperView.Model.init : stepper_view;

                 let stepper_view =
                   calculate_stepper(
                     ~settings=stepper_settings,
                     ~sem_ctx,
                     ~ana=Some(Calc.OldValue(Typ.fresh(Atom(Bool)))),
                     ~goal_exp,
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

    let explores =
      explores
      |> {
        let.calc dynamics = dynamics;
        let explores =
          switch (dynamics) {
          | None => []
          | Some(d) => d.explores
          };
        List.map(((id, _, _)) => id, explores) |> List.rev;
      }
      |> Calc.old_if_same'(explores);
    let explore_ids = Calc.get_value(explores);
    let explore_map =
      Id.Map.filter((id, _) => List.mem(id, explore_ids), explore_map);

    let explore_map =
      dynamics
      |> Calc.get_value
      |> (
        fun
        | None => []
        | Some(x) => x.explores
      )
      |> List.fold_left(
           (acc, (id, env', exp)) => {
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
                   Option.value(~default=Model.theorem_init("explore"), opt);

                 let goal_changed = has_changed_goal(goal_exp, exp);

                 let goal_exp = Calc.set(~eq=Exp.fast_equal, exp, goal_exp);

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
                     let env = env_with_symbolic_ctx_vars(ctx, env);
                     SemanticCtx.of_ctx_and_env(ctx, env);
                   };

                 let stepper_view =
                   goal_changed ? StepperView.Model.init : stepper_view;

                 let stepper_view = {
                   calculate_stepper(
                     ~settings=stepper_settings,
                     ~sem_ctx,
                     ~ana=None,
                     ~goal_exp,
                     stepper_view,
                   );
                 };

                 Some({
                   name: "explore",
                   ctx: ctx |> Calc.save,
                   env: env |> Calc.save,
                   sem_ctx: sem_ctx |> Calc.save,
                   goal_exp: goal_exp |> Calc.save,
                   stepper_view,
                 });
               },
               acc,
             )
           },
           explore_map,
         );

    Model.{
      thm_map,
      thms: thms |> Calc.save,
      explore_map,
      explores: explores |> Calc.save,
    };
  };
};

module Focus = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TheoremFocus(int, StepperView.Focus.t)
    | ExploreFocus(int, StepperView.Focus.t);

  let get_cursor_info = (~inject, ~focus: t, model: Model.t) => {
    switch (focus) {
    | TheoremFocus(idx, step_focus) =>
      let id_and_thm = {
        open OptUtil.Syntax;
        let* id = List.nth_opt(model.thms |> Calc.get_saved([]), idx);
        let* thm = Id.Map.find_opt(id, model.thm_map);
        Some((id, thm));
      };
      switch (id_and_thm) {
      | Some((_id, thm)) =>
        let+ c =
          StepperView.Focus.get_cursor_info(
            ~inject=x => inject(Update.TheoremUpdate(idx, x)),
            ~focus=step_focus,
            thm.stepper_view,
          );
        Update.TheoremUpdate(idx, c);
      | None => Cursor.empty
      };
    | ExploreFocus(idx, step_focus) =>
      let id_and_explore = {
        open OptUtil.Syntax;
        let* id = List.nth_opt(model.explores |> Calc.get_saved([]), idx);
        let* explore = Id.Map.find_opt(id, model.explore_map);
        Some((id, explore));
      };
      switch (id_and_explore) {
      | Some((_id, explore)) =>
        let+ c =
          StepperView.Focus.get_cursor_info(
            ~inject=x => inject(Update.ExploreUpdate(idx, x)),
            ~focus=step_focus,
            explore.stepper_view,
          );
        Update.ExploreUpdate(idx, c);
      | None => Cursor.empty
      };
    };
  };
};

module View = {
  open WebUtil;

  let view_stepper =
      (
        ~globals,
        ~take_focus,
        ~inject,
        ~selected,
        ~focus_of_stepper,
        ~update_of_stepper,
        stepper_view,
      ) =>
    StepperView.View.view(
      ~globals,
      ~signal=
        fun
        | MakeActive(f) => take_focus(focus_of_stepper(f))
        | HideStepper => Ui_effect.Ignore,
      ~inject=a => inject(update_of_stepper(a)),
      ~selected,
      ~is_toplevel=false,
      stepper_view,
    );

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Focus.t => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        model: Model.t,
      ) => {
    let stepper_globals = {
      ...globals,
      settings: proof_settings(globals.settings),
    };
    let theorem_views =
      List.mapi(
        (idx, id) => {
          switch (Id.Map.find_opt(id, model.thm_map)) {
          | None => None
          | Some(Model.{stepper_view, name, _}) =>
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
              view_stepper(
                ~globals=stepper_globals,
                ~take_focus,
                ~inject,
                ~selected=
                  switch (selected) {
                  | Some(TheoremFocus(idx', s)) when idx == idx' => Some(s)
                  | _ => None
                  },
                ~focus_of_stepper=f => TheoremFocus(idx, f),
                ~update_of_stepper=a => Update.TheoremUpdate(idx, a),
                stepper_view,
              );
            Some(div_c("theorem", [header, ...stepper]));
          }
        },
        model.thms |> Calc.get_saved([]),
      )
      |> List.filter_map(Fun.id);
    let explore_views =
      List.mapi(
        (idx, id) => {
          switch (Id.Map.find_opt(id, model.explore_map)) {
          | None => None
          | Some(explore: Model.theorem) =>
            let stepper_view = explore.stepper_view;
            let header =
              WebUtil.div_c(
                "theorem-header",
                [Node.strong([Node.text("Explore expression")])],
              );
            let stepper =
              view_stepper(
                ~globals=stepper_globals,
                ~take_focus,
                ~inject,
                ~selected=
                  switch (selected) {
                  | Some(ExploreFocus(idx', s)) when idx == idx' => Some(s)
                  | _ => None
                  },
                ~focus_of_stepper=f => ExploreFocus(idx, f),
                ~update_of_stepper=a => Update.ExploreUpdate(idx, a),
                stepper_view,
              );
            Some(div_c("theorem", [header, ...stepper]));
          }
        },
        model.explores |> Calc.get_saved([]),
      )
      |> List.filter_map(Fun.id);
    theorem_views @ explore_views;
  };
};
