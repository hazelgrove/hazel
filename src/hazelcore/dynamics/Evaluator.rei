[@deriving sexp]
type invalid_input =
  | FreeOrInvalidVariable(DHExp.t)
  | CastBVHoleGround(DHExp.t)
  | ApInvalidBoxedFunctionVal(DHExp.t)
  | BoxedNotIntLit2(DHExp.t)
  | BoxedNotIntLit1(DHExp.t)
  | BoxedNotFloatLit1(DHExp.t)
  | BoxedNotFloatLit2(DHExp.t);

exception InvalidInput(invalid_input);

[@deriving sexp]
type result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

[@deriving sexp]
type state = EvalState.t;

[@deriving sexp]
type report = (result, state);

let evaluate: (~state: state=?, DHExp.t) => report;

/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;
