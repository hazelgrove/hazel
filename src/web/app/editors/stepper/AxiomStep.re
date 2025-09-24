open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  name: string,
  at_idx: int,
  at_exp: Exp.t,
  direction: Direction.t,
  equality: string,
  next_exp: Calc.saved(Exp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = {
  name: string,
  at_idx: int,
  at_exp: Exp.t,
  direction: Direction.t,
  equality: string,
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

  let persist = (model: model): persistent => {
    {
      name: model.name,
      at_idx: model.at_idx,
      at_exp: model.at_exp,
      direction: model.direction,
      equality: model.equality,
    };
  };

  let unpersist = (p: persistent): model => {
    {
      name: p.name,
      at_idx: p.at_idx,
      at_exp: p.at_exp,
      direction: p.direction,
      equality: p.equality,
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
        ~settings as _: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~state: Calc.t(EvaluatorState.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map,
        ~ana as _,
        model: model,
      ) => {
    let {name, at_idx, at_exp, direction, equality, next_exp} = model;
    let+ next_exp =
      next_exp
      |> Calc.map_saved(Option.some)
      |> {
        let.calc exp = exp
        and.calc ctx = ctx
        and.calc info_map = info_map;
        let* e = ProofHacks.nth_exp(at_exp, at_idx, exp);
        let proof_ctx =
          ProofCtx.of_env(
            ~builtins=Axioms.v,
            ~ctx=SemanticCtx.get_ctx(ctx),
            SemanticCtx.get_env(ctx),
          );
        let* proofrule = ProofCtx.lookup_rule(equality, proof_ctx);
        let (l, r) =
          ProofRule.can_eq(
            ~info_map,
            ~env=SemanticCtx.get_env(ctx),
            proofrule,
            e,
          );
        let* with_exp =
          switch (direction) {
          | Left => l
          | Right => r
          };
        Some(ProofHacks.replace_exp_id(e |> DHExp.rep_id, exp, with_exp));
      }
      |> Calc.to_option;
    (
      {
        name,
        at_idx,
        at_exp,
        direction,
        equality,
        next_exp: next_exp |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some((next_exp, state)),
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
    WebUtil.Node.text(m.name);

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
