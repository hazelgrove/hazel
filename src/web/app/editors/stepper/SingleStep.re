open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the module to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  evalobj: EvaluatorStep.step,
  next_exp: Calc.saved(Exp.t),
  next_state: Calc.saved(EvaluatorState.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  |;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  |;

/* The methods in this file, like the other step files, are
   parameterized by a Stepper module that implements the
   stepper interface. This allows us to use steppers inside
   steps inside steppers. The lines below can be copied as
   boilerplate to other steps.*/
module F =
       (Stepper: STEPPER)

         : (
           STEP with
             type model = model'(Stepper.model) and
             type action = action'(Stepper.action) and
             type focus = focus'(Stepper.focus)
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model'(Stepper.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action'(Stepper.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = focus'(Stepper.focus);

  let update = (~settings as _: Settings.t, action: action, _model: model) =>
    switch (action) {
    | _ => .
    };

  let can_undo = _ => false;

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx as _: Calc.t(Ctx.t),
        ~env: Calc.t(ClosureEnvironment.t),
        ~state: Calc.t(EvaluatorState.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~ana as _,
        model: model,
      ) => {
    let {evalobj, next_exp, next_state} = model;
    let* hidden_and_eo =
      Calc.pair_saved(hidden, Calculated(evalobj))
      |> Calc.map_saved(Option.some)
      |> {
        let.calc settings = settings
        and.calc exp = exp
        and.calc env = env
        and.calc state = state;
        let+ (filter_action, eo) =
          EvaluatorStep.refresh_step(~settings, exp, env, state, evalobj);
        let hidden =
          switch (filter_action) {
          | FilterAction.Step => false
          | FilterAction.Eval => true
          };
        (hidden, eo);
      }
      |> Calc.to_option;
    let (hidden, evalobj) = Calc.to_pair(hidden_and_eo);
    let+ next_exp_and_state =
      Calc.pair_saved(next_exp, next_state)
      |> Calc.map_saved(Option.some)
      |> {
        let.calc evalobj = evalobj;
        EvaluatorStep.take_step(evalobj);
      }
      |> Calc.to_option;
    let (next_exp, next_state) = Calc.to_pair(next_exp_and_state);
    (
      {
        evalobj: evalobj |> Calc.get_value,
        next_exp: next_exp |> Calc.save,
        next_state: next_state |> Calc.save,
      },
      hidden,
      Some((next_exp, next_state)),
      Calc.OldValue(Some(true)),
    );
  };

  let get_cursor_info = (~focus: focus, _model: model) =>
    switch (focus) {
    | _ => .
    };

  let handle_key_event = (~focus: focus, ~event as _: Key.t, _model: model) =>
    switch (focus) {
    | _ => .
    };

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        m: model,
      ) =>
    WebUtil.Node.text(
      m.evalobj
      |> EvaluatorStep.get_step_kind
      |> Transition.stepper_justification,
    );

  let view_content =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        _model: model,
      ) =>
    [];
};
