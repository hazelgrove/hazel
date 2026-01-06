open Util;
open Language;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_elab_subst: Calc.saved(Exp.t),
    ctx: Calc.saved(Ctx.t),
    root: StepperBase.step_model,
  };

  let init = {
    cached_elab_subst: Calc.Pending,
    ctx: Calc.Pending,
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
      ctx: Calc.Pending,
      root: StepperBase.Stepper.unpersist(p.root),
    };
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_action;

  let update = (~globals, action, model: Model.t) => {
    open Updated;
    let* root = StepperBase.Stepper.update(~globals, action, model.root);
    {
      ...model,
      root,
    };
  };

  let calculate =
      (
        ~globals: Globals.t,
        ~settings: Calc.t(CoreSettings.t),
        elab: Calc.t(Exp.t),
        {ctx, cached_elab_subst, root}: Model.t,
      )
      : Model.t => {
    let ctx = ctx |> Calc.const(() => Builtins.ctx_init(None));
    let elab_subst =
      cached_elab_subst
      |> {
        open Calc.Syntax;
        let.calc elab = elab;
        Substitution.subst(Builtins.env_init, elab);
      };
    let state = Calc.OldValue(EvaluatorState.init);
    let root =
      StepperBase.Stepper.calculate(
        ~globals,
        ~settings,
        ~ctx,
        ~exp=elab_subst,
        ~state,
        root,
      )
      |> fst;
    {
      cached_elab_subst: elab_subst |> Calc.save,
      ctx: ctx |> Calc.save,
      root,
    };
  };

  let can_undo = StepperBase.Stepper.can_undo;
};

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_focus;

  let get_cursor_info =
      (
        ~globals: Globals.t,
        ~inject: Update.t => Ui_effect.t(unit),
        ~focus: t,
        model: Model.t,
      )
      : Haz3lcore.Cursor.t => {
    StepperBase.Stepper.get_cursor_info(
      ~globals,
      ~inject,
      ~focus,
      model.root,
    );
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
      ~is_toplevel=true,
      ~focus=selected,
      model.root,
    )
    @ settings_modal;
  };
};
