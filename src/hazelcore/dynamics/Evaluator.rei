[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

[@deriving sexp]
type result = EvaluatorResult.t;

[@deriving sexp]
type state = EvalState.t;

[@deriving sexp]
type report = (result, state);

let evaluate: (~state: state=?, DHExp.t) => report;

/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;

let unbox_result: result => DHExp.t;
