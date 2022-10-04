[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(Typ.t) /* the argument is the corresponding ground type */;

/**
  [evaluate builtins env d] is [(es, r)], where [r] is the result of evaluating [d] and
  [es] is the accumulated state.
 */
let evaluate:
  (Environment.t, DHExp.t) => (EvaluatorState.t, EvaluatorResult.t);

/**
  [evaluate_top env d] is [(es, env', r)], where [es] is the accumulated
  state, [r] is the result of evaluating [d], and [env'] is the environment
  produced by the top-level let expressions in [d].
 */
let evaluate_top:
  (Environment.t, DHExp.t) =>
  (EvaluatorState.t, Environment.t, EvaluatorResult.t);
