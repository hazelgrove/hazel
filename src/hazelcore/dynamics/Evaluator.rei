[@deriving sexp]
type result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let evaluate: (Environment.t, DHExp.t) => result;

/* closed substitution [d1/x]d2*/
// TODO: remove
/* let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

   let subst: (Environment.t, DHExp.t) => DHExp.t; */
