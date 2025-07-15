open Util;
open Language;
open Haz3lcore;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  at_id: Id.t,
  at_exp: Exp.t,
  with_exp: Exp.t,
  next_exp: Calc.saved(Exp.t),
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
        ~settings as _: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx as _: Calc.t(Ctx.t),
        ~state: Calc.t(EvaluatorState.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        model: model,
      ) => {
    let {at_id, at_exp, with_exp, next_exp} = model;
    let+ next_exp =
      next_exp
      |> Calc.map_saved(Option.some)
      |> {
        let.calc exp = exp;
        let* _ = ProofHacks.find_exp_id(at_id, exp);
        Some(ProofHacks.replace_exp_id(at_id, exp, with_exp));
      }
      |> Calc.to_option;
    (
      {
        at_id,
        at_exp,
        with_exp,
        next_exp: next_exp |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some((next_exp, state)),
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
        _: model,
      ) =>
    WebUtil.Node.text("Axiom Step");

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
