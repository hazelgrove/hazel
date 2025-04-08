// INVARIANT: this evaluate function should never return an expression with closures.

let evaluate:
  (~step_limit: int=?, ~env: Environment.t, Exp.t) =>
  (Exp.t, EvaluatorState.t);
