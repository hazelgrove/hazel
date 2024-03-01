/**
  // TODO[Matt]: find where this comment belongs
  [evaluate builtins env d] is [(es, r)], where [r] is the result of evaluating [d] and
  [es] is the accumulated state.
 */
open Transition;

let evaluate:
  (Environment.t, Elaborator.Elaboration.t) =>
  (EvaluatorState.t, EvaluatorResult.t);

module EvaluatorEVMode: {
  type result_unfinished =
    | BoxedValue(DExp.t)
    | Indet(DExp.t)
    | Uneval(DExp.t);

  let unbox: result_unfinished => DExp.t;

  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and type result = result_unfinished;
};

module Eval: {
  let transition:
    (
      (EvaluatorEVMode.state, ClosureEnvironment.t, DExp.t) =>
      EvaluatorEVMode.result_unfinished,
      EvaluatorEVMode.state,
      ClosureEnvironment.t,
      DExp.t
    ) =>
    EvaluatorEVMode.result_unfinished;
};
