/**
  Monad for the evaluator.
 */

include Util.StateMonad.S with type state = EvaluatorState.t;

/**
  See {!val:EvaluatorState.take_step}
 */
let take_step: t(unit);

let add_test: (KeywordID.t, TestMap.instance_report) => t(unit);
