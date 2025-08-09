open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  inner_exp: Calc.saved(Exp.t),
  bindings: Calc.saved(Ctx.t),
  inner_stepper: 'stepper,
  result_function: Calc.saved(Exp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | InnerExp('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | InnerExp('step);

let init = init_step => {
  inner_exp: Calc.Pending,
  bindings: Calc.Pending,
  inner_stepper: init_step,
  result_function: Calc.Pending,
};

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

  let update = (~settings: Settings.t, action: action, model: model) => {
    Updated.(
      switch (action) {
      | InnerExp(a) =>
        let* new_inner_step =
          Stepper.update(~settings, a, model.inner_stepper);
        {
          ...model,
          inner_stepper: new_inner_step,
        };
      }
    );
  };

  let can_undo = (a: action) =>
    switch (a) {
    | InnerExp(step) => Stepper.can_undo(step)
    };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(Ctx.t),
        ~state: Calc.t(EvaluatorState.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        model: model,
      ) => {
    let {inner_exp, bindings, inner_stepper, result_function} = model;
    let+ (bindings, inner_exp) =
      (bindings, inner_exp)
      |> Calc.saved_pair
      |> Calc.map_saved(Option.some)
      |> {
        let.calc exp = exp
        and.calc ctx = ctx;
        switch (exp |> Exp.term_of) {
        | Fun(p, d1, t, _) =>
          let t =
            OptUtil.get(
              () => Typ.fresh(Unknown(Internal |> Prov.anonymous)),
              t,
            );
          let* bindings = ProofHacks.dhpat_extend_ctx(p, t, ctx);
          Some((bindings, d1));
        | _ => None
        };
      }
      |> Calc.to_option
      |> Option.map(Calc.to_pair);
    let (inner_stepper, last) =
      Stepper.calculate(
        ~settings,
        ~ctx=bindings,
        ~exp=inner_exp,
        ~state,
        inner_stepper,
      );
    let result_function =
      result_function
      |> {
        let.calc last = last
        and.calc exp = exp;
        switch (exp |> Exp.term_of) {
        | Fun(p, _, t, n) => DHExp.fresh(Fun(p, last, t, n))
        | _ =>
          DHExp.fresh(
            Fun(
              Pat.fresh(EmptyHole),
              last,
              Some(Typ.fresh(Unknown(Internal |> Prov.anonymous))),
              None,
            ),
          )
        };
      };
    (
      {
        inner_exp: inner_exp |> Calc.save,
        bindings: bindings |> Calc.save,
        inner_stepper,
        result_function: result_function |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some((result_function, state)),
    );
  };

  let get_cursor_info = (~focus: focus, model: model) =>
    Cursor.(
      switch (focus) {
      | InnerExp(a) =>
        let+ ci = Stepper.get_cursor_info(~focus=a, model.inner_stepper);
        (InnerExp(ci): action);
      }
    );

  let handle_key_event =
      (~focus: focus, ~event: Key.t, model: model): option(action) =>
    switch (focus) {
    | InnerExp(a) =>
      Stepper.handle_key_event(~focus=a, ~event, model.inner_stepper)
      |> Option.map((x): action => InnerExp(x))
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
    WebUtil.Node.text("Forall Step");

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        model: model,
      ) => {
    let inner_stepper =
      Stepper.view(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(InnerExp(f)) => Some(f)
          | None => None
          },
        ~inject=x => inject(InnerExp(x)),
        ~take_focus=x => take_focus(InnerExp(x)),
        ~hide_stepper,
        ~is_toplevel,
        model.inner_stepper,
      );

    inner_stepper;
  };
};
