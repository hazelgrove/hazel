open Util;
open Calc.Syntax;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type theorem = {
    ctx: Calc.saved(Ctx.t),
    // env: Environment.t,
    goal_exp: Calc.saved(Exp.t),
    stepper_view: StepperView.Model.t,
  };

  let theorem_init = {
    ctx: Calc.Pending,
    goal_exp: Calc.Pending,
    stepper_view: StepperView.Model.init,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    thm_map: Id.Map.t(theorem),
    thms: Calc.saved(list(Id.t)),
  };

  let init = {
    thm_map: Id.Map.empty,
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
    let thms =
      thms
      |> {
        let.calc dynamics = dynamics;
        let theorems =
          switch (dynamics) {
          | None => []
          | Some(d) => d.theorems
          };
        List.map(((id, _, _)) => id, theorems) |> List.rev;
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
      |> List.fold_left(
           (acc, (id, _, goal_typ)) =>
             Id.Map.update(
               id,
               (opt: option(Model.theorem)) => {
                 let Model.{ctx, goal_exp, stepper_view} =
                   Option.value(~default=Model.theorem_init, opt);

                 let goal_exp =
                   Calc.set(
                     ~eq=Exp.fast_equal,
                     ProofHacks.goal_of_typ(goal_typ),
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
                     |> Option.value(~default=Ctx.empty);
                   };

                 let stepper_view =
                   StepperView.Update.calculate(
                     ~settings,
                     ~ctx,
                     goal_exp,
                     stepper_view,
                   );

                 Some({
                   ctx: ctx |> Calc.save,
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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Id.t, StepperView.Focus.t);
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
    switch (model.thms |> Calc.get_saved_exc) {
    | [] => [Node.text("No theorems found")]
    | xs =>
      List.map(
        id => {
          let Model.{stepper_view, _} = Id.Map.find(id, model.thm_map);
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
            stepper_view,
          );
        },
        xs,
      )
      |> List.flatten
    };
  };
};
