open Util;
open Calc.Syntax;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type theorem = {
    name: string,
    ctx: Calc.saved(Ctx.t),
    env: Calc.saved(ClosureEnvironment.t),
    goal_exp: Calc.saved(Exp.t),
    stepper_view: StepperView.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_theorem = {stepper_view: StepperView.Model.persistent};

  let theorem_init = name => {
    name,
    ctx: Calc.Pending,
    env: Calc.Pending,
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
            goal_exp: Calc.Pending,
            stepper_view: StepperView.Model.unpersist(p_thm.stepper_view),
          },
        p.thm_map,
      ),
    thms: Calc.Pending,
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TheoremUpdate(Id.t, StepperView.Update.t);

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
    | TheoremUpdate(id, action) =>
      switch (Id.Map.find_opt(id, model.thm_map)) {
      | Some(thm) =>
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
      | None => model |> Updated.return_quiet
      }
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
          List.filter_map(
            ((a, b, c, d)) => {
              open OptUtil.Syntax;
              let* d' = ProofRule.typ_to_rule(d);
              Some((a, b, c, d'));
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
      |> List.filter_map(((a, b, c, d)) => {
           open OptUtil.Syntax;
           let* d' = ProofRule.typ_to_rule(d);
           Some((a, b, c, d'));
         })
      |> List.fold_left(
           (acc, (id, name, env', rule: ProofRule.t)) =>
             Id.Map.update(
               id,
               (opt: option(Model.theorem)) => {
                 let Model.{name: _, ctx, env, goal_exp, stepper_view} =
                   Option.value(~default=Model.theorem_init("?"), opt);

                 let goal_exp =
                   Calc.set(
                     ~eq=Exp.fast_equal,
                     rule |> ProofRule.conclusion_exp,
                     goal_exp,
                   );

                 let ctx =
                   ctx
                   |> {
                     let.calc statics = statics;
                     statics.info_map
                     |> Statics.Map.lookup(id)
                     |> Option.bind(
                          _,
                          fun
                          | Info.InfoExp({ctx, _}) => Some(ctx)
                          | _ => None,
                        )
                     |> Option.value(~default=Ctx.empty)
                     |> List.fold_left(
                          Ctx.extend,
                          _,
                          rule.bindings |> List.rev,
                        );
                   };

                 let env =
                   Calc.set(~eq=ClosureEnvironment.id_equal, env', env);

                 let stepper_view =
                   StepperView.Update.calculate(
                     ~settings,
                     ~ctx,
                     ~env,
                     ~ana=Calc.OldValue(Typ.fresh(Atom(Bool))),
                     goal_exp,
                     stepper_view,
                   );

                 Some({
                   name,
                   ctx: ctx |> Calc.save,
                   env: env |> Calc.save,
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
  type t = (Id.t, StepperView.Focus.t);

  let get_cursor_info = (~focus: t, model: Model.t) =>
    switch (Id.Map.find_opt(focus |> fst, model.thm_map)) {
    | Some(thm) =>
      let+ c =
        StepperView.Focus.get_cursor_info(
          ~focus=snd(focus),
          thm.stepper_view,
        );
      Update.TheoremUpdate(focus |> fst, c);
    | None => Cursor.empty
    };

  let handle_key_event = (~focus: t, ~event: Key.t, model: Model.t) =>
    switch (Id.Map.find_opt(focus |> fst, model.thm_map)) {
    | Some(thm) =>
      StepperView.Focus.handle_key_event(
        ~focus=snd(focus),
        ~event,
        thm.stepper_view,
      )
      |> Option.map((x): Update.t => Update.TheoremUpdate(fst(focus), x))
    | None => None
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
    switch (model.thms |> Calc.get_saved_exc) {
    | [] => []
    | xs =>
      List.map(
        id => {
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
                | MakeActive(f) => take_focus((id, f))
                | HideStepper => Ui_effect.Ignore,
              ~inject=a => inject(Update.TheoremUpdate(id, a)),
              ~selected=
                switch (selected) {
                | Some((id', s)) when Id.equal(id, id') => Some(s)
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
