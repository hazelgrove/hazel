open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the module to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  // Constant
  persistent_evalobj: EvaluatorStep.persistent,
  // Calculated
  evalobj: Calc.saved(EvaluatorStep.step),
  next_exp: Calc.saved(Exp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = EvaluatorStep.persistent;

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
             type persistent = persistent'(Stepper.persistent) and
             type action = action'(Stepper.action) and
             type focus = focus'(Stepper.focus)
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model'(Stepper.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = persistent'(Stepper.persistent);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action'(Stepper.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = focus'(Stepper.focus);

  let persist = (model: model) => {
    model.persistent_evalobj;
  };

  let unpersist = (p: persistent) => {
    {
      persistent_evalobj: p,
      evalobj: Calc.Pending,
      next_exp: Calc.Pending,
    };
  };

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
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        model: model,
      ) => {
    let {persistent_evalobj, evalobj, next_exp} = model;
    let* hidden_and_eo =
      Calc.pair_saved(hidden, evalobj)
      |> Calc.map_saved(Option.some)
      |> {
        let.calc settings = settings
        and.calc exp = exp;

        let+ (filter_action, eo) =
          EvaluatorStep.refresh_step(~settings, exp, persistent_evalobj);
        let hidden =
          switch (filter_action) {
          | FilterAction.Step => false
          | FilterAction.Eval => true
          };
        (hidden, eo);
      }
      |> Calc.to_option;
    let (hidden, evalobj) = Calc.to_pair(hidden_and_eo);
    let+ next_exp =
      next_exp
      |> Calc.map_saved(Option.some)
      |> {
        let.calc evalobj = evalobj;
        EvaluatorStep.take_step(evalobj);
      }
      |> Calc.to_option;
    (
      {
        persistent_evalobj,
        evalobj: evalobj |> Calc.save,
        next_exp: next_exp |> Calc.save,
      },
      hidden,
      Some(next_exp),
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
      |> Calc.get_saved_exc(~print="EvaluatorStep not calculated")
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
