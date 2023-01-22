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

let get_id: t(IdGen.state);

let put_id: IdGen.state => t(unit);

let with_id: (IdGen.state => ('a, IdGen.state)) => t('a);

/**
  See {!val:EvaluatorState.take_step}
 */
let take_step: t(unit);

let add_test: (KeywordID.t, TestMap.instance_report) => t(unit);
