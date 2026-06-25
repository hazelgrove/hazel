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

let exp_to_code = (~settings: CoreSettings.t, exp: Exp.t): string =>
  exp
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=
         Haz3lcore.ExpToSegment.Settings.of_core(~inline=true, settings),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~indent="")
  |> StringUtil.trim_trailing_whitespace;

let metadata_prefix = "hazel-explore-stepper:";

let encode_stepper_metadata = (stepper: StepperView.Model.persistent): string =>
  stepper
  |> StepperView.Model.yojson_of_persistent
  |> Yojson.Safe.to_string
  |> StringUtil.compress;

let decode_stepper_metadata =
    (comment: string): option(StepperView.Model.persistent) => {
  let comment = String.trim(comment);
  let len = String.length(comment);
  let comment =
    if (len >= 2
        && String.sub(comment, 0, 1) == "#"
        && String.sub(comment, len - 1, 1) == "#") {
      String.sub(comment, 1, len - 2);
    } else {
      comment;
    };
  let prefix_len = String.length(metadata_prefix);
  if (String.length(comment) > prefix_len
      && String.sub(comment, 0, prefix_len) == metadata_prefix) {
    let encoded =
      String.sub(comment, prefix_len, String.length(comment) - prefix_len);
    try(
      encoded
      |> StringUtil.decompress
      |> Yojson.Safe.from_string
      |> StepperView.Model.persistent_of_yojson
      |> Option.some
    ) {
    | _ => None
    };
  } else {
    None;
  };
};

let stepper_metadata_comment = (stepper: StepperView.Model.persistent): string =>
  "#" ++ metadata_prefix ++ encode_stepper_metadata(stepper) ++ "#";

let stepper_metadata_of_exp =
    (exp: Exp.t): option(StepperView.Model.persistent) => {
  let comments = ref([]);
  let collect_secondary = (secondary: Secondary.t) =>
    switch (secondary.content) {
    | Comment(text) => comments := [text, ...comments^]
    | Whitespace(_) => ()
    };
  let collect_annotation = (exp: Exp.t) => {
    let (before, after) = exp.annotation.secondary;
    List.iter(collect_secondary, before);
    List.iter(collect_secondary, after);
  };
  let f_exp = (continue, exp: Exp.t) => {
    collect_annotation(exp);
    continue(exp);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  comments^ |> List.find_map(decode_stepper_metadata);
};

let promote_explore_code =
    (
      ~settings: CoreSettings.t,
      ~name: string,
      ~original: Exp.t,
      ~landed: Exp.t,
      ~stepper: StepperView.Model.persistent,
    )
    : string => {
  let original_code = exp_to_code(~settings, original);
  let landed_code = exp_to_code(~settings, landed);
  "theorem "
  ++ name
  ++ " = "
  ++ original_code
  ++ " == "
  ++ landed_code
  ++ " in "
  ++ stepper_metadata_comment(stepper);
};

let promote_explore_code_with_metadata =
    (
      ~settings: CoreSettings.t,
      ~name: string,
      ~original: Exp.t,
      ~landed: Exp.t,
      ~stepper: StepperView.Model.persistent,
    )
    : string =>
  promote_explore_code(~settings, ~name, ~original, ~landed, ~stepper);

let promote_explore_goal = (~original: Exp.t, ~landed: Exp.t): Exp.t =>
  Exp.fresh(BinOp(Poly(Equals), original, landed));

let reflexivity_step_for = (exp: Exp.t): StepperBase.persistent_step =>
  StepperBase.{
    step_kind:
      AxiomStep({
        name: "Reflexive(==)",
        at_idx: 0,
        at_exp: exp,
        direction: Direction.Right,
        equality: "Reflexive(==)",
      }),
    next_step: None,
  };

let rec replace_terminal_missing_step =
        (
          replacement: StepperBase.persistent_step,
          step: StepperBase.persistent_step,
        )
        : StepperBase.persistent_step => {
  switch (step.next_step) {
  | Some(next_step) => {
      ...step,
      next_step: Some(replace_terminal_missing_step(replacement, next_step)),
    }
  | None =>
    switch (step.step_kind) {
    | MissingStep(_) => replacement
    | _ => {
        ...step,
        next_step: Some(replacement),
      }
    }
  };
};

let stepper_with_final_reflexivity =
    (~landed: Exp.t, stepper: StepperView.Model.persistent)
    : StepperView.Model.persistent => {
  let reflexive_goal = Exp.fresh(BinOp(Poly(Equals), landed, landed));
  {
    root:
      replace_terminal_missing_step(
        reflexivity_step_for(reflexive_goal),
        stepper.root,
      ),
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type promoted_explore_stepper = {
    name: string,
    goal: Exp.t,
    stepper: StepperView.Model.persistent,
  };

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
    promoted_explore_steppers: list(promoted_explore_stepper),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {thm_map: Id.Map.t(persistent_theorem)};

  let init = {
    thm_map: Id.Map.empty,
    thms: Calc.Pending,
    explore_map: Id.Map.empty,
    explores: Calc.Pending,
    promoted_explore_steppers: [],
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
    promoted_explore_steppers: [],
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
    | ExploreUpdate(int, StepperView.Update.t)
    | PromoteExplore(
        Id.t,
        string,
        string,
        Exp.t,
        StepperView.Model.persistent,
      );

  let can_undo = (action: t) => {
    switch (action) {
    | TheoremUpdate(_, action) => StepperView.Update.can_undo(action)
    | ExploreUpdate(_, action) => StepperView.Update.can_undo(action)
    | PromoteExplore(_, _, _, _, _) => true
    };
  };

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | TheoremUpdate(n, action) =>
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
    | ExploreUpdate(n, action) =>
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
      let id_and_explore = {
        open OptUtil.Syntax;
        let* id = List.nth_opt(model.explores |> Calc.get_saved([]), n);
        let* explore = Id.Map.find_opt(id, model.explore_map);
        Some((id, explore));
      };
      switch (id_and_explore) {
      | Some((id, explore)) =>
        let* stepper_view =
          StepperView.Update.update(~settings, action, explore.stepper_view);
        let explore_map =
          Id.Map.add(
            id,
            {
              ...explore,
              stepper_view,
            },
            model.explore_map,
          );
        Model.{
          ...model,
          explore_map,
        };
      | None => model |> Updated.raise_invalid_action
      };
    | PromoteExplore(_, _, name, goal, stepper) =>
      Model.{
        ...model,
        promoted_explore_steppers: [
          {
            name,
            goal,
            stepper,
          },
          ...model.promoted_explore_steppers,
        ],
      }
      |> Updated.return_quiet
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~statics: Calc.t(Haz3lcore.CachedStatics.t),
        ~dynamics: Calc.t(option(Dynamics.t)),
        {thm_map, thms, explore_map, explores, promoted_explore_steppers}: Model.t,
      ) => {
    let theorem_settings' = {
      ...Calc.get_value(settings),
      evaluation: {
        ...Calc.get_value(settings).evaluation,
        enable_proof: true,
        stepper_history: true,
      },
    };
    let theorem_settings =
      switch (settings) {
      | OldValue(_) => Calc.OldValue(theorem_settings')
      | NewValue(_) => Calc.NewValue(theorem_settings')
      };
    let explore_settings' = {
      ...Calc.get_value(settings),
      evaluation: {
        ...Calc.get_value(settings).evaluation,
        enable_proof: true,
        stepper_history: true,
      },
    };
    let explore_settings =
      switch (settings) {
      | OldValue(_) => Calc.OldValue(explore_settings')
      | NewValue(_) => Calc.NewValue(explore_settings')
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
                 let transient_stepper =
                   List.find_opt(
                     (seed: Model.promoted_explore_stepper) =>
                       Equality.ignoring_ascriptions.exp(
                         seed.goal,
                         conclusion_exp,
                       )
                       || seed.name == name,
                     promoted_explore_steppers,
                   )
                   |> Option.map((seed: Model.promoted_explore_stepper) =>
                        seed.stepper
                      );
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
                 let source_stepper =
                   switch (
                     Statics.Map.lookup_exp(
                       id,
                       Calc.get_value(statics).info_map,
                     )
                   ) {
                   | Some(info) => stepper_metadata_of_exp(info.user_term)
                   | None => None
                   };
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
                   | Some(thm) =>
                     switch (transient_stepper) {
                     | Some(stepper) => seeded_theorem(stepper)
                     | None => thm
                     }
                   | None =>
                     let stepper =
                       switch (transient_stepper) {
                       | Some(_) as stepper => stepper
                       | None =>
                         switch (carried_stepper) {
                         | Some(_) as stepper => stepper
                         | None => source_stepper
                         }
                       };
                     switch (stepper) {
                     | Some(stepper) => seeded_theorem(stepper)
                     | None => Model.theorem_init("?")
                     };
                   };

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
                   StepperView.Update.calculate(
                     ~settings=theorem_settings,
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

                 let stepper_view = {
                   StepperView.Update.calculate(
                     ~settings=explore_settings,
                     ~ctx=sem_ctx,
                     goal_exp,
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

    let promoted_explore_steppers =
      switch (Calc.get_value(dynamics)) {
      | None => promoted_explore_steppers
      | Some(_) =>
        let theorem_goals =
          dynamics
          |> Calc.get_value
          |> (
            fun
            | None => []
            | Some(x) => x.theorems
          )
          |> List.map(((_, theorem_name, _, goal)) =>
               (
                 theorem_name,
                 goal
                 |> Substitution.in_exp(Environment.empty)
                 |> ProofRule.exp_to_rule
                 |> ProofRule.conclusion_exp,
               )
             );
        List.filter(
          (seed: Model.promoted_explore_stepper) =>
            !
              List.exists(
                ((theorem_name, theorem_goal)) =>
                  Equality.ignoring_ascriptions.exp(seed.goal, theorem_goal)
                  || seed.name == theorem_name,
                theorem_goals,
              ),
          promoted_explore_steppers,
        );
      };

    Model.{
      thm_map,
      thms: thms |> Calc.save,
      explore_map,
      explores: explores |> Calc.save,
      promoted_explore_steppers,
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

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Focus.t => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        model: Model.t,
      ) => {
    let theorem_globals = {
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
    let explore_globals = {
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
    let theorem_views =
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
              ~globals=theorem_globals,
              ~signal=
                fun
                | MakeActive(f) => take_focus(TheoremFocus(idx, f))
                | HideStepper => Ui_effect.Ignore,
              ~inject=a => inject(Update.TheoremUpdate(idx, a)),
              ~selected=
                switch (selected) {
                | Some(TheoremFocus(idx', s)) when idx == idx' => Some(s)
                | _ => None
                },
              ~is_toplevel=false,
              stepper_view,
            );
          div_c("theorem", [header, ...stepper]);
        },
        model.thms |> Calc.get_saved([]),
      );
    let explore_views =
      List.mapi(
        (idx, id) => {
          let explore: Model.theorem = Id.Map.find(id, model.explore_map);
          let stepper_view = explore.stepper_view;
          let promote_button =
            switch (
              explore.goal_exp |> Calc.get_saved_opt,
              StepperView.Model.get_landed_exp(stepper_view),
            ) {
            | (Some(original), Some(landed)) =>
              let default_name =
                "th_"
                ++ string_of_int(
                     List.length(model.thms |> Calc.get_saved([])) + 1,
                   );
              [
                Node.button(
                  ~attrs=[
                    Attr.create("type", "button"),
                    Attr.class_("theorem-promote-button"),
                    Attr.title("Rewrite this explore as a theorem"),
                    Attr.on_click(_ => {
                      let name =
                        switch (JsUtil.prompt("Theorem name", default_name)) {
                        | Some(name) =>
                          let name = String.trim(name);
                          name == "" ? default_name : name;
                        | None => default_name
                        };
                      let stepper =
                        stepper_view
                        |> StepperView.Model.persist
                        |> stepper_with_final_reflexivity(~landed);
                      let code =
                        promote_explore_code(
                          ~settings=globals.settings.core,
                          ~name,
                          ~original,
                          ~landed,
                          ~stepper,
                        );
                      let goal = promote_explore_goal(~original, ~landed);
                      inject(
                        Update.PromoteExplore(id, code, name, goal, stepper),
                      );
                    }),
                  ],
                  [Node.text("make theorem")],
                ),
              ];
            | _ => []
            };
          let header =
            WebUtil.div_c(
              "theorem-header",
              [
                Node.strong([Node.text("Explore expression")]),
                ...promote_button,
              ],
            );
          let stepper =
            StepperView.View.view(
              ~globals=explore_globals,
              ~signal=
                fun
                | MakeActive(f) => take_focus(ExploreFocus(idx, f))
                | HideStepper => Ui_effect.Ignore,
              ~inject=a => inject(Update.ExploreUpdate(idx, a)),
              ~selected=
                switch (selected) {
                | Some(ExploreFocus(idx', s)) when idx == idx' => Some(s)
                | _ => None
                },
              ~is_toplevel=false,
              stepper_view,
            );
          div_c("theorem", [header, ...stepper]);
        },
        model.explores |> Calc.get_saved([]),
      );
    theorem_views @ explore_views;
  };
};
