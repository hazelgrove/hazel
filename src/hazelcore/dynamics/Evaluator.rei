[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

/**
  [evaluate env d] is [(es, r)], where [r] is the result of evaluating [d] and
  [es] is the accumulated state.
 */
let evaluate:
  (Environment.t, DHExp.t) => (EvaluatorState.t, EvaluatorResult.t);

/* closed substitution [d1/x]d2;
   Not needed for evaluation with environments,
   leaving in case it's useful for something else */
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;
let subst: (Environment.t, DHExp.t) => DHExp.t;
