/**
  Monad for the evaluator.
 */

include Util.StateMonad.S with type state = EvaluatorState.t;

/**
  See {!val:EvaluatorState.get_eig}.
 */
let get_eig: t(EnvironmentIdGen.t);

/**
  See {!val:EvaluatorState.put_eig}.
 */
let put_eig: EnvironmentIdGen.t => t(unit);

/**
  See {!val:EvaluatorState.with_eig}.
 */
let with_eig: (EnvironmentIdGen.t => ('a, EnvironmentIdGen.t)) => t('a);

/**
  See {!val:EvaluatorState.take_step}
 */
let take_step: t(unit);
let reset_step: t(unit);
let get_step: t(int);

let time_out: t(bool);

let add_test: (KeywordID.t, TestMap.instance_report) => t(unit);
