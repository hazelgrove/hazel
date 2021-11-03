[@deriving sexp]
type eval_error =
  | OutOfFuel
  | FreeInvalidVar(Var.t)
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t)
  | InvalidBoxedLam(DHExp.t)
  | InvalidBoxedBoolLit(DHExp.t)
  | InvalidBoxedIntLit(DHExp.t)
  | InvalidBoxedFloatLit(DHExp.t);

[@deriving sexp]
type result =
  | InvalidInput(eval_error)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let evaluate: DHExp.t => result;

/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;
