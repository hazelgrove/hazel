open Util;
open Language;
open StepInterface;
open Calc.Syntax;

/* `revert <exp> => <proof>` as a stepper row
 * (docs/prover-obligations.md, Phase 4c). Like Forall, this is a WRAPPING
 * proof form: it owns a body proof rather than continuing a `Seq` chain,
 * so the row renders the keyword and its reverted expression and then
 * hands the body to a nested stepper — structurally the same shape as
 * ForallStep.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* The body's goal and scope. The goal comes from the checker (the
   * body's own ProofMap `incoming`): `F ==> G` when the reverted fact was
   * found, the un-reverted goal when it was not (recovery). The scope is
   * the enclosing one unchanged — `revert` MOVES a fact into the goal
   * without removing it from context, which is what makes the ex-falso
   * idiom (revert, then rewrite with the other facts) work. */
  inner_exp: Calc.saved(Exp.t),
  inner_ctx: Calc.saved(SemanticCtx.t),
  inner_stepper: 'stepper,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | InnerExp('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | InnerExp('step)
  /* The form's own expression argument, edited in place as a SubEditor
   * window onto the main editor (see ProofFormView.view_arg). Carries
   * no local model: the splice IS the proof text, so this constructor
   * only records which sub-view holds focus. */
  | Arg(CodeEditable.Selection.t);

let init = init_step => {
  inner_exp: Calc.Pending,
  inner_ctx: Calc.Pending,
  inner_stepper: init_step,
};

/* The reverted expression, read from syntax (single source of truth). */
let arg_of_proof = (proof: option(Proof.t)): option(Exp.t) =>
  switch (proof) {
  | Some({term: Revert(e, _), _}) => Some(e)
  | _ => None
  };

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
        ~hidden as _: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map as _,
        ~proof_info_map as _,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {inner_exp, inner_ctx, inner_stepper} = model;
    /* Descend into the body proof: the inner stepper operates on `body`,
     * not on the Revert node itself (otherwise inner-stepper actions
     * would target / replace the Revert, destroying its structure). */
    let descend = (p: Proof.t): Proof.t =>
      switch (p) {
      | {term: Revert(_, body), _} => body
      | p => p
      };
    let inner_proof =
      switch (proof) {
      | OldValue(p) => Calc.OldValue(descend(p))
      | NewValue(p) => Calc.NewValue(descend(p))
      };
    /* The body's goal, as the checker computed it. No entry yet (nothing
     * checked, or an upstream step failed) falls back to this row's own
     * expression — the same goal an unmatched `revert` passes through. */
    let inner_exp =
      inner_exp
      |> {
        let.calc exp = exp
        and.calc inner_proof = inner_proof
        and.calc proof_map = proof_map;
        switch (ProofMap.lookup(Proof.rep_id(inner_proof), proof_map)) {
        | Some({incoming: Some(goal), _}) => goal
        | _ => exp
        };
      };
    /* Mirror `ProofCheck`'s Revert scope exactly: unchanged. The fact
     * stays citable inside the body (that is the whole point of the
     * form), so the nested rows see the enclosing context. */
    let inner_ctx =
      inner_ctx
      |> {
        let.calc ctx = ctx;
        ctx;
      };
    let inner_stepper =
      Stepper.calculate(
        ~settings,
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~ana,
        ~proof=inner_proof,
        ~proof_map,
        inner_stepper,
      );
    Some({
      inner_exp: inner_exp |> Calc.save,
      inner_ctx: inner_ctx |> Calc.save,
      inner_stepper,
    });
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
      /* The arg editor's actions belong to the main editor (they are
       * injected through its own channel), so this row contributes no
       * cursor info of its own. */
      | Arg(_) => Cursor.empty
      }
    );

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~is_toplevel as _: bool,
        ~proof as _: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        _: model,
      ) =>
    WebUtil.Node.text("Revert");

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~is_toplevel: bool,
        ~proof: option(Proof.t),
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
          | Some(Arg(_))
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
    ProofFormView.view_arg(
      ~globals,
      ~label="Revert: ",
      ~proof,
      ~main_editor,
      ~focused=
        switch (focus) {
        | Some(Arg(_)) => true
        | Some(InnerExp(_))
        | None => false
        },
      ~take_focus=() => take_focus(Arg()),
      arg_of_proof(proof),
    )
    @ inner_stepper;
  };
};
