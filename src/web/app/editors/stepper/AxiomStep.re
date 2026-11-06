open Util;
open Language;
open StepInterface;

/* A rewrite with an equality, an axiom or an in-scope fact. The row reads
 * the rewritten goal off the checker's ProofMap; this kind only names the
 * row and marks the rewritten sub-term. Both come from the proof syntax.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  equality: string,
  at_exp: Exp.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) = Inert.action'('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) = Inert.focus'('step);

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

  include Inert;

  let calculate =
      (
        ~settings as _: Calc.t(CoreSettings.t),
        ~exp as _: Calc.t(Exp.t),
        ~ctx as _: Calc.t(SemanticCtx.t),
        ~info_map as _: Calc.t(Statics.Map.t),
        ~proof_info_map as _: Calc.t(Statics.Map.t),
        ~ana as _: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map as _: Calc.t(ProofMap.t),
        model: model,
      ) =>
    switch (Calc.get_value(proof)) {
    | {term: AxiomStep({at_exp, equality, _}), _} =>
      Some({
        equality:
          ProofSyntax.exp_to_name(equality)
          |> Option.value(~default=model.equality),
        at_exp,
      })
    | _ => Some(model)
    };

  let justification = (m: model) => m.equality;
};
