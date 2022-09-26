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
  (Builtins.forms, Environment.t, DHExp.t) =>
  (EvaluatorState.t, EvaluatorResult.t);
