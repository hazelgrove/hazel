open Transition;

let evaluate:
  (Environment.t, Elaborator.Elaboration.t) =>
  (EvaluatorState.t, EvaluatorResult.t);

module EvaluatorEVMode: {
  type result_unfinished =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t)
    | Uneval(DHExp.t);

  let unbox: result_unfinished => DHExp.t;

  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and type result = result_unfinished;
};

module Eval: {
  let transition:
    (
      (EvaluatorEVMode.state, ClosureEnvironment.t, DHExp.t) =>
      EvaluatorEVMode.result_unfinished,
      EvaluatorEVMode.state,
      ClosureEnvironment.t,
      DHExp.t
    ) =>
    EvaluatorEVMode.result_unfinished;
};
