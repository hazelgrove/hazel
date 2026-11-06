open Util;
open Language;
open StepInterface;

/* A step whose row adds nothing but its justification (`eval`,
 * `contradiction`): the row's expressions come from the proof syntax and
 * the checker's ProofMap, which the surrounding row reads. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) = Inert.action'('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) = Inert.focus'('step);

module F =
       (Label: {
          let justification: string;
        }, Stepper: STEPPER)

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
        ~proof as _: Calc.t(Proof.t),
        ~proof_map as _: Calc.t(ProofMap.t),
        model: model,
      ) =>
    Some(model);

  let justification = () => Label.justification;
};
