/**
  [evaluate builtins env d] is [(es, r)], where [r] is the result of evaluating [d] and
  [es] is the accumulated state.
 */
let evaluate:
  (Environment.t, DHExp.t) => (EvaluatorState.t, EvaluatorResult.t);
