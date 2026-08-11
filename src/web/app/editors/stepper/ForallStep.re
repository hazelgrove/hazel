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
  inner_ctx: Calc.saved(SemanticCtx.t),
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
  inner_ctx: Calc.Pending,
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
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map as _,
        ~proof_info_map as _,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(option(Proof.t)),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {inner_exp, inner_ctx, inner_stepper, result_function} = model;
    let+ (inner_ctx, inner_exp) =
      (inner_ctx, inner_exp)
      |> Calc.saved_pair
      |> Calc.map_saved(Option.some)
      |> {
        let.calc exp = exp
        and.calc ctx = ctx;
        switch (exp |> Exp.term_of) {
        | Fun(p, d1, t, _) =>
          let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
          Some((SemanticCtx.add_from_pattern(ctx, p, t), d1));
        | _ => None
        };
      }
      |> Calc.to_option
      |> Option.map(Calc.to_pair);
    /* Descend into the body proof: if a `Forall(_, body)` is in scope,
     * the inner stepper operates on `body` rather than the outer node
     * (otherwise inner stepper actions would target / replace the
     * Forall itself, destroying its structure). */
    let descend = (p: option(Proof.t)): option(Proof.t) =>
      switch (p) {
      | Some({term: Forall(_, body), _}) => Some(body)
      | _ => p
      };
    let inner_proof =
      switch (proof) {
      | OldValue(p) => Calc.OldValue(descend(p))
      | NewValue(p) => Calc.NewValue(descend(p))
      };
    let (inner_stepper, last, validity) =
      Stepper.calculate(
        ~settings,
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~ana,
        ~proof=inner_proof,
        ~proof_map,
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
              Some(Typ.fresh(Unknown(Internal))),
              None,
            ),
          )
        };
      };
    (
      {
        inner_exp: inner_exp |> Calc.save,
        inner_ctx: inner_ctx |> Calc.save,
        inner_stepper,
        result_function: result_function |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some(result_function),
      validity,
    );
  };

  let get_cursor_info = (~inject, ~focus: focus, model: model) =>
    Cursor.(
      switch (focus) {
      | InnerExp(a) =>
        let+ ci =
          Stepper.get_cursor_info(
            ~inject=a => inject(InnerExp(a): action),
            ~focus=a,
            model.inner_stepper,
          );
        (InnerExp(ci): action);
      }
    );

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        ~proof as _: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
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
        ~proof as _: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
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
        ~edit_syntax,
        ~main_editor,
        model.inner_stepper,
      );

    inner_stepper;
  };
};
