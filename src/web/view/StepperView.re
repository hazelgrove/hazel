open Util;
open Language;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_elab_subst: Calc.saved(Exp.t),
    root: StepperBase.step_model,
  };

  let init = {
    cached_elab_subst: Calc.Pending,
    root: StepperBase.init_step,
  };

  let get_state = (m: t) => StepperBase.Stepper.get_state(m.root);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {root: StepperBase.persistent_step};

  let persist = (model: t): persistent => {
    root: StepperBase.Stepper.persist(model.root),
  };

  let unpersist = (p: persistent): t => {
    {
      cached_elab_subst: Calc.Pending,
      root: StepperBase.Stepper.unpersist(p.root),
    };
  };

  let get_validity = (m: t) => StepperBase.Stepper.get_validity(m.root);
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_action;

  let update = (~settings, action, model: Model.t) => {
    open Updated;
    let* root = StepperBase.Stepper.update(~settings, action, model.root);
    {
      ...model,
      root,
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~ctx: Calc.t(Ctx.t),
        ~env: Calc.t(ClosureEnvironment.t),
        elab: Calc.t(Exp.t),
        ~ana=Calc.OldValue(Typ.fresh(Unknown(SynSwitch))),
        {cached_elab_subst, root}: Model.t,
      )
      : Model.t => {
    let elab_subst =
      cached_elab_subst
      |> {
        open Calc.Syntax;
        let.calc elab = elab
        and.calc env = env;
        Substitution.subst(env |> ClosureEnvironment.map_of, elab);
      };
    let state = Calc.OldValue(EvaluatorState.init);
    let (root, _, _) =
      StepperBase.Stepper.calculate(
        ~settings,
        ~ctx,
        ~exp=elab_subst,
        ~env,
        ~state,
        ~ana,
        root,
      );
    {
      cached_elab_subst: elab_subst |> Calc.save,
      root,
    };
  };

  let can_undo = StepperBase.Stepper.can_undo;
};

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_focus;

  open Cursor;

  let get_cursor_info = (~focus: t, model: Model.t): cursor(Update.t) => {
    let+ ci = StepperBase.Stepper.get_cursor_info(~focus, model.root);
    ci;
  };

  let handle_key_event = (~focus: t, ~event, model: Model.t) =>
    StepperBase.Stepper.handle_key_event(~focus, ~event, model.root);
};

module View = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type event =
    | MakeActive(Focus.t)
    | HideStepper;

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        ~is_toplevel=true,
        model: Model.t,
      ) => {
    let settings_modal =
      globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            globals.settings.core.evaluation,
          )
        : [];
    StepperBase.Stepper.view(
      ~globals,
      ~take_focus=f => signal(MakeActive(f)),
      ~hide_stepper=signal(HideStepper),
      ~inject=u => inject(u),
      ~is_toplevel,
      ~focus=selected,
      model.root,
    )
    @ settings_modal;
  };
};
