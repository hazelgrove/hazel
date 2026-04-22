open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* UI proof-step module for `eval at ... on ... end`.
 *
 * Mirrors AlgebriteStep but computes the replacement by taking a single
 * dynamic evaluation step on the target subexpression (via
 * EvaluatorStep.get_status/take_step) rather than using a
 * user-specified `with_exp`. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  at_idx: int,
  at_exp: Exp.t,
  next_exp: Calc.saved(Exp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  |;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  |;

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

  /* Single-step callback using the dynamic evaluator stepper. Matches
   * the one in Evaluator.re used by ProofCheck.check. Silently
   * advances through auto-hidden steps (RemoveParens, CaseApply, ...)
   * *and* `VarLookup` (which is cosmetic for proof purposes), then
   * takes one user-visible transition and again skips any trailing
   * silent steps. */
  let is_proof_silent = (s: EvaluatorStep.step) =>
    switch (EvaluatorStep.get_step_kind(s)) {
    | VarLookup => true
    | _ => false
    };
  let rec advance_hidden = (~env, e): (list((Exp.t, string, Exp.t)), Exp.t) =>
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
    | AutoStep(s) =>
      switch (EvaluatorStep.take_step(s)) {
      | Some(next) =>
        let (rest, outgoing) = advance_hidden(~env, next);
        let justification =
          s |> EvaluatorStep.get_step_kind |> Transition.stepper_justification;
        ([(e, justification, next), ...rest], outgoing);
      | None => ([], e)
      }
    | AvailableSteps([s, ..._]) when is_proof_silent(s) =>
      switch (EvaluatorStep.take_step(s)) {
      | Some(next) =>
        let (rest, outgoing) = advance_hidden(~env, next);
        let justification =
          s |> EvaluatorStep.get_step_kind |> Transition.stepper_justification;
        ([(e, justification, next), ...rest], outgoing);
      | None => ([], e)
      }
    | AvailableSteps(_) => ([], e)
    };
  let step_fn: ProofCheck.step_fn =
    (~env, e) => {
      let (leading_auto_steps, e) = advance_hidden(~env, e);
      switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
      | AvailableSteps([s, ..._]) =>
        switch (EvaluatorStep.take_step(s)) {
        | Some(next) =>
          let (trailing_auto_steps, outgoing) = advance_hidden(~env, next);
          Some(
            ProofCheck.{
              auto_incoming:
                List.map(
                  ((_, justification, output)) => (justification, output),
                  leading_auto_steps,
                ),
              auto_outgoing:
                List.map(
                  ((input, justification, _)) => (input, justification),
                  trailing_auto_steps,
                ),
              outgoing,
            },
          );
        | None => None
        }
      | AutoStep(_)
      | AvailableSteps([]) => None
      };
    };

  let calculate =
      (
        ~settings as _: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map as _,
        ~proof_info_map as _,
        ~ana as _,
        ~proof: Calc.t(option(Proof.t)),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {at_idx, at_exp, next_exp} = model;
    /* Mirror AxiomStep: when a Proof.t sub-term is in scope, override
     * `at_idx` / `at_exp` from the `EvalStep` proof node so the display
     * follows the syntax (this lets a freshly-patched proof surface
     * through the placeholder model that `Stepper.calculate` inserts
     * via `adapt_step_kind`). */
    let (at_idx, at_exp) =
      switch (Calc.get_value(proof)) {
      | Some({term: EvalStep({at_idx: ai, at_exp: ae}), _}) =>
        let idx = ProofCheck.exp_to_int(ai) |> Option.value(~default=at_idx);
        (idx, ae);
      | _ => (at_idx, at_exp)
      };
    let+ next_exp =
      next_exp
      |> Calc.map_saved(Option.some)
      |> {
        let.calc exp = exp
        and.calc ctx = ctx
        and.calc proof = proof
        and.calc proof_map = proof_map;
        /* Prefer the ProofMap outgoing entry (already computed by the
         * big-step evaluator) when this step has one; otherwise re-run
         * the local single-step evaluator as before. */
        switch (proof) {
        | Some(p) =>
          switch (ProofMap.lookup(Proof.rep_id(p), proof_map)) {
          | Some({outgoing: Some(_) as outgoing, _}) => outgoing
          | _ =>
            ProofCheck.eval_step_outgoing(
              ~step=step_fn,
              ~env=SemanticCtx.get_env(ctx),
              ~at_idx,
              ~at_exp,
              exp,
            )
          }
        | None =>
          ProofCheck.eval_step_outgoing(
            ~step=step_fn,
            ~env=SemanticCtx.get_env(ctx),
            ~at_idx,
            ~at_exp,
            exp,
          )
        };
      }
      |> Calc.to_option;
    (
      {
        at_idx,
        at_exp,
        next_exp: next_exp |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some(next_exp),
      Calc.OldValue(Some(true)),
    );
  };

  let get_cursor_info = (~inject as _, ~focus: focus, _model: model) =>
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
        ~proof as _: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        _m: model,
      ) =>
    WebUtil.Node.text("eval");

  let view_content =
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
        ~main_editor as _: option(CodeEditable.Channel.t),
        _model: model,
      ) =>
    [];
};
