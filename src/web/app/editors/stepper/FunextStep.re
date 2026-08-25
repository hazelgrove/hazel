open Util;
open Language;
open StepInterface;
open Calc.Syntax;

/* `funext x => <proof>` as a stepper row: functional extensionality as an
 * INTRO rule (never an axiom — an axiom is a rewrite equality, and funext
 * as one would have to quantify over functions at unknown/hole types).
 *
 * Like Forall and Assume this is a WRAPPING proof form: it owns a body
 * proof rather than continuing a `Seq` chain, so the row renders the
 * keyword and its binder and then hands the body to a nested stepper —
 * structurally the same shape as ForallStep.
 *
 * The one structural difference from ForallStep: `forall x =>` takes its
 * binder from the GOAL (the goal already quantifies), while `funext x =>`
 * takes it from the KEYWORD (the goal `f == g` names no argument). So the
 * pattern comes off the proof term, not off `peel_binder`.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* The body's pointwise goal `f(x) == g(x)` and the scope that binds
   * `x`. The goal is read from the checker's own ProofMap `incoming` for
   * the body, so the two cannot drift; the scope mirrors ProofCheck's
   * `add_from_pattern(ctx, pat, domain)`. */
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
  /* The form's own binder PATTERN, edited in place as a SubEditor window
   * onto the main editor (see ProofFormView.view_pat_arg). Carries no
   * local model: the splice IS the proof text, so this constructor only
   * records which sub-view holds focus. */
  | Arg(CodeEditable.Selection.t);

let init = init_step => {
  inner_exp: Calc.Pending,
  inner_ctx: Calc.Pending,
  inner_stepper: init_step,
};

/* The binder, read from syntax (single source of truth). */
let arg_of_proof = (proof: option(Proof.t)): option(Pat.t) =>
  switch (proof) {
  | Some({term: Funext(p, _), _}) => Some(p)
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
        ~info_map: Calc.t(Statics.Map.t),
        ~proof_info_map as _,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {inner_exp, inner_ctx, inner_stepper} = model;
    /* Descend into the body proof: the inner stepper operates on `body`,
     * not on the Funext node itself (otherwise inner-stepper actions
     * would target / replace the Funext, destroying its structure). */
    let descend = (p: Proof.t): Proof.t =>
      switch (p) {
      | {term: Funext(_, body), _} => body
      | p => p
      };
    let inner_proof =
      switch (proof) {
      | OldValue(p) => Calc.OldValue(descend(p))
      | NewValue(p) => Calc.NewValue(descend(p))
      };
    /* The pointwise goal, as the checker computed it. No entry yet
     * (nothing checked, or an upstream step failed) falls back to this
     * row's own expression — which is also what the malformed-goal
     * recovery passes through. */
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
    /* Mirror ProofCheck's Funext scope: the keyword's binder at the
     * equality's DOMAIN type. `funext_goal` is the same detector the
     * checker uses, so the row and the rule cannot disagree about
     * whether this goal is a function equality. */
    let inner_ctx =
      inner_ctx
      |> {
        let.calc ctx = ctx
        and.calc exp = exp
        and.calc info_map = info_map
        and.calc proof = proof;
        switch (proof.term) {
        | Funext(pat, _) =>
          let env = SemanticCtx.get_env(ctx);
          let domain =
            switch (ProofCheck.funext_goal(~info_map, ~env, exp)) {
            | Some((_, _, d)) => d
            | None => Typ.fresh(Unknown(Internal))
            };
          SemanticCtx.add_from_pattern(ctx, pat, domain);
        | _ => ctx
        };
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
      /* The binder editor's actions belong to the main editor (they are
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
    WebUtil.Node.text("extensionality");

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
    /* The binder chip sits ABOVE the nested stepper: `funext x =>` reads
     * left-to-right as "name the argument, then prove the body", and the
     * body's own rows are what the nested stepper draws. */
    ProofFormView.view_pat_arg(
      ~globals,
      ~label="argument: ",
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
