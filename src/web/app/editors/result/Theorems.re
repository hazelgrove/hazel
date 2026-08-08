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
  type stepper = {
    name: string,
    ctx: Calc.saved(Ctx.t),
    env: Calc.saved(Environment.t(Exp.t)),
    sem_ctx: Calc.saved(SemanticCtx.t),
    goal_exp: Calc.saved(Exp.t),
    stepper_view: StepperView.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_theorem = {stepper_view: StepperView.Model.persistent};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type item =
    | TheoremItem(Id.t)
    | ExploreItem(Id.t);

  let stepper_init = (~math_policy, name) => {
    name,
    ctx: Calc.Pending,
    env: Calc.Pending,
    sem_ctx: Calc.Pending,
    goal_exp: Calc.Pending,
    stepper_view:
      StepperView.Model.init
      |> StepperView.Model.with_math_policy(math_policy),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    thm_map: Id.Map.t(stepper),
    explore_map: Id.Map.t(stepper),
    items: Calc.saved(list(item)),
    math_policy: option(ExerciseMathPolicy.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    thm_map: Id.Map.t(persistent_theorem),
    [@yojson.default Id.Map.empty] [@sexp.default Id.Map.empty]
    explore_map: Id.Map.t(persistent_theorem),
  };

  let init = {
    thm_map: Id.Map.empty,
    explore_map: Id.Map.empty,
    items: Calc.Pending,
    math_policy: None,
  };

  let with_math_policy = (math_policy, model: t): t => {
    ...model,
    math_policy,
    thm_map:
      model.thm_map
      |> Id.Map.map((stepper: stepper) =>
           {
             ...stepper,
             stepper_view:
               stepper.stepper_view
               |> StepperView.Model.with_math_policy(math_policy),
           }
         ),
    explore_map:
      model.explore_map
      |> Id.Map.map((stepper: stepper) =>
           {
             ...stepper,
             stepper_view:
               stepper.stepper_view
               |> StepperView.Model.with_math_policy(math_policy),
           }
         ),
  };

  let persist = (model: t): persistent => {
    thm_map:
      Id.Map.map(
        (thm: stepper): persistent_theorem =>
          {stepper_view: StepperView.Model.persist(thm.stepper_view)},
        model.thm_map,
      ),
    explore_map:
      Id.Map.map(
        (explore: stepper): persistent_theorem =>
          {stepper_view: StepperView.Model.persist(explore.stepper_view)},
        model.explore_map,
      ),
  };

  let unpersist = (p: persistent): t => {
    thm_map:
      Id.Map.map(
        (p_thm: persistent_theorem): stepper =>
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
    explore_map:
      Id.Map.map(
        (p_explore: persistent_theorem): stepper =>
          {
            name: "explore",
            ctx: Calc.Pending,
            env: Calc.Pending,
            sem_ctx: Calc.Pending,
            goal_exp: Calc.Pending,
            stepper_view: StepperView.Model.unpersist(p_explore.stepper_view),
          },
        p.explore_map,
      ),
    items: Calc.Pending,
    math_policy: None,
  };

  let get_score = (model: t): option((float, float)) => {
    open OptUtil.Syntax;
    let* items = model.items |> Calc.get_saved_opt;
    let thms =
      List.filter_map(
        fun
        | TheoremItem(id) => Some(id)
        | ExploreItem(_) => None,
        items,
      );
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

  let get_explore_score =
      (~settings, ~target, model: t): option((float, float)) => {
    open OptUtil.Syntax;
    let* items = model.items |> Calc.get_saved_opt;
    let* id =
      items
      |> List.find_map(
           fun
           | ExploreItem(id) => Some(id)
           | TheoremItem(_) => None,
         );
    let* explore = Id.Map.find_opt(id, model.explore_map);
    let* terminal = StepperView.Model.terminal_exp(explore.stepper_view);
    let* sem_ctx = explore.sem_ctx |> Calc.get_saved_opt;
    let exact = Equality.ignoring_ascriptions.exp(terminal, target);
    let certified =
      exact
      || Option.is_some(
           RewriteChecker.check_written_step_trace_for_profile(
             ~stage=explore.stepper_view.automation_stage,
             ~profile=StepperView.Model.active_profile(explore.stepper_view),
             ~settings,
             ~env=SemanticCtx.get_env(sem_ctx),
             terminal,
             target,
           ),
         );
    Some((certified ? 1.0 : 0.0, 1.0));
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TheoremUpdate(Id.t, StepperView.Update.t)
    | ExploreUpdate(Id.t, StepperView.Update.t);

  let can_undo = (action: t) => {
    switch (action) {
    | TheoremUpdate(_, action)
    | ExploreUpdate(_, action) => StepperView.Update.can_undo(action)
    };
  };

  let update_stepper =
      (~settings, ~action, stepper: Model.stepper): Updated.t(Model.stepper) => {
    let* stepper_view =
      StepperView.Update.update(~settings, action, stepper.stepper_view);
    {
      ...stepper,
      stepper_view,
    };
  };

  let calculate_stepper =
      (
        ~settings,
        ~sem_ctx,
        ~ana: option(Calc.t(Typ.t))=?,
        ~goal_exp,
        stepper_view,
      )
      : StepperView.Model.t =>
    StepperView.Update.calculate(
      ~settings,
      ~ctx=sem_ctx,
      ~ana?,
      goal_exp,
      stepper_view,
    );

  let has_changed_goal = (previous: Calc.saved(Exp.t), current: Exp.t): bool =>
    switch (previous |> Calc.get_saved_opt) {
    | Some(previous) => !Equality.ignoring_ascriptions.exp(previous, current)
    | None => false
    };

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    let settings = proof_settings(settings);
    switch (action) {
    | TheoremUpdate(id, action) =>
      switch (Id.Map.find_opt(id, model.thm_map)) {
      | Some(thm) =>
        let* thm = update_stepper(~settings, ~action, thm);
        let thm_map = Id.Map.add(id, thm, model.thm_map);
        Model.{
          ...model,
          thm_map,
        };
      | None => model |> Updated.raise_invalid_action
      }
    | ExploreUpdate(id, action) =>
      switch (Id.Map.find_opt(id, model.explore_map)) {
      | Some(explore) =>
        let* explore = update_stepper(~settings, ~action, explore);
        let explore_map = Id.Map.add(id, explore, model.explore_map);
        Model.{
          ...model,
          explore_map,
        };
      | None => model |> Updated.raise_invalid_action
      }
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~statics: Calc.t(Haz3lcore.CachedStatics.t),
        ~dynamics: Calc.t(option(Dynamics.t)),
        {thm_map, explore_map, items, math_policy}: Model.t,
      ) => {
    let stepper_settings = proof_calc_settings(settings);
    let items =
      items
      |> {
        let.calc dynamics = dynamics;
        switch (dynamics) {
        | None => []
        | Some(d) =>
          d.stepper_items
          |> List.map(
               fun
               | Dynamics.TheoremStepper(id, _, _, _) =>
                 Model.TheoremItem(id)
               | Dynamics.ExploreStepper(id, _, _) => Model.ExploreItem(id),
             )
          |> List.rev
        };
      }
      |> Calc.old_if_same'(items);
    let thm_ids =
      Calc.get_value(items)
      |> List.filter_map(
           fun
           | Model.TheoremItem(id) => Some(id)
           | Model.ExploreItem(_) => None,
         );
    let explore_ids =
      Calc.get_value(items)
      |> List.filter_map(
           fun
           | Model.TheoremItem(_) => None
           | Model.ExploreItem(id) => Some(id),
         );
    let thm_map = Id.Map.filter((id, _) => List.mem(id, thm_ids), thm_map);
    let explore_map =
      Id.Map.filter((id, _) => List.mem(id, explore_ids), explore_map);

    let stepper_items =
      switch (Calc.get_value(dynamics)) {
      | None => []
      | Some(d) => d.stepper_items
      };

    // Calculate visible steppers
    let thm_map =
      stepper_items
      |> List.filter_map(
           fun
           | Dynamics.TheoremStepper(id, name, env, exp) =>
             Some((
               id,
               name,
               env,
               exp
               |> Substitution.in_exp(Environment.empty)
               |> ProofRule.exp_to_rule,
             ))
           | Dynamics.ExploreStepper(_, _, _) => None,
         )
      |> List.fold_left(
           (acc, (id, name, env', rule: ProofRule.t)) =>
             Id.Map.update(
               id,
               (opt: option(Model.stepper)) => {
                 let conclusion_exp = rule |> ProofRule.conclusion_exp;
                 let Model.{
                   name: _,
                   ctx,
                   env,
                   sem_ctx,
                   goal_exp,
                   stepper_view,
                 } =
                   Option.value(
                     ~default=Model.stepper_init(~math_policy, "?"),
                     opt,
                   );

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
                   goal_changed
                     ? Model.stepper_init(~math_policy, name).stepper_view
                     : stepper_view;

                 let stepper_view =
                   calculate_stepper(
                     ~settings=stepper_settings,
                     ~sem_ctx,
                     ~ana=Calc.OldValue(Typ.fresh(Atom(Bool))),
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

    let explore_map =
      stepper_items
      |> List.filter_map(
           fun
           | Dynamics.TheoremStepper(_, _, _, _) => None
           | Dynamics.ExploreStepper(id, env, exp) => Some((id, env, exp)),
         )
      |> List.fold_left(
           (acc, (id, env', exp)) => {
             Id.Map.update(
               id,
               (opt: option(Model.stepper)) => {
                 let Model.{
                   name: _,
                   ctx,
                   env,
                   sem_ctx,
                   goal_exp,
                   stepper_view,
                 } =
                   Option.value(
                     ~default=Model.stepper_init(~math_policy, "explore"),
                     opt,
                   );

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
                   goal_changed
                     ? Model.stepper_init(~math_policy, "explore").
                         stepper_view
                     : stepper_view;

                 let stepper_view =
                   calculate_stepper(
                     ~settings=stepper_settings,
                     ~sem_ctx,
                     ~goal_exp,
                     stepper_view,
                   );

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
      explore_map,
      items: items |> Calc.save,
      math_policy,
    };
  };
};

module Focus = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TheoremFocus(Id.t, StepperView.Focus.t)
    | ExploreFocus(Id.t, StepperView.Focus.t);

  let get_cursor_info = (~inject, ~focus: t, model: Model.t) => {
    switch (focus) {
    | TheoremFocus(id, step_focus) =>
      switch (Id.Map.find_opt(id, model.thm_map)) {
      | Some(thm) =>
        let+ c =
          StepperView.Focus.get_cursor_info(
            ~inject=x => inject(Update.TheoremUpdate(id, x)),
            ~focus=step_focus,
            thm.stepper_view,
          );
        Update.TheoremUpdate(id, c);
      | None => Cursor.empty
      }
    | ExploreFocus(id, step_focus) =>
      switch (Id.Map.find_opt(id, model.explore_map)) {
      | Some(explore) =>
        let+ c =
          StepperView.Focus.get_cursor_info(
            ~inject=x => inject(Update.ExploreUpdate(id, x)),
            ~focus=step_focus,
            explore.stepper_view,
          );
        Update.ExploreUpdate(id, c);
      | None => Cursor.empty
      }
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
    model.items
    |> Calc.get_saved([])
    |> List.filter_map(
         fun
         | Model.TheoremItem(id) =>
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
                   | Some(TheoremFocus(id', s)) when Id.equal(id, id') =>
                     Some(s)
                   | _ => None
                   },
                 ~focus_of_stepper=f => TheoremFocus(id, f),
                 ~update_of_stepper=a => Update.TheoremUpdate(id, a),
                 stepper_view,
               );
             Some(div_c("theorem", [header, ...stepper]));
           }
         | Model.ExploreItem(id) =>
           switch (Id.Map.find_opt(id, model.explore_map)) {
           | None => None
           | Some(explore: Model.stepper) =>
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
                   | Some(ExploreFocus(id', s)) when Id.equal(id, id') =>
                     Some(s)
                   | _ => None
                   },
                 ~focus_of_stepper=f => ExploreFocus(id, f),
                 ~update_of_stepper=a => Update.ExploreUpdate(id, a),
                 stepper_view,
               );
             Some(div_c("theorem", [header, ...stepper]));
           },
       );
  };
};
