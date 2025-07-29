open Util;
open Language;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_settings: Calc.saved(CoreSettings.t),
    cached_elab: Calc.saved(Exp.t),
    cached_elab_subst: Calc.saved(Exp.t),
    ctx: Calc.saved(Ctx.t),
    root: StepperBase.step_model,
  };

  let init = {
    cached_settings: Calc.Pending,
    cached_elab: Calc.Pending,
    cached_elab_subst: Calc.Pending,
    ctx: Calc.Pending,
    root: StepperBase.init_step,
  };

  let get_state = (m: t) => StepperBase.Stepper.get_state(m.root);

  let get_elaboration = (m: t) => m.cached_elab |> Calc.saved_to_option;
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
        ~settings,
        elab: Exp.t,
        {ctx, cached_settings, cached_elab, cached_elab_subst, root}: Model.t,
      )
      : Model.t => {
    let settings =
      cached_settings
      |> Calc.set(settings, ~eq=(a, b) => {
           CoreSettings.{
             ...a,
             evaluation: {
               ...a.evaluation,
               show_settings: true,
               stepper_history: true,
             },
           }
           == CoreSettings.{
                ...b,
                evaluation: {
                  ...b.evaluation,
                  show_settings: true,
                  stepper_history: true,
                },
              }
         });
    let elab = cached_elab |> Calc.set(~eq=Exp.fast_equal, elab);
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
        ~settings,
        ~ctx,
        ~exp=elab_subst,
        ~state,
        root,
      )
      |> fst;
    {
      cached_settings: settings |> Calc.save,
      cached_elab: elab |> Calc.save,
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
