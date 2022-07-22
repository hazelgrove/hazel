[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let evaluate:
  (ClosureEnvironment.t, DHExp.t) => (EvaluatorState.t, EvaluatorResult.t);

/* closed substitution [d1/x]d2;
   Not needed for evaluation with environments,
   leaving in case it's useful for something else */
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;
let subst: (Environment.t, DHExp.t) => DHExp.t;
