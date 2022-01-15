[@deriving sexp]
type result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let evaluate: (EvalEnv.EvalEnvCtx.t, EvalEnv.t, DHExp.t) => result;

/* closed substitution [d1/x]d2;
   Not needed for evaluation with environments,
   leaving in case it's useful for something else */
/* let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;
   let subst: (Environment.t, DHExp.t) => DHExp.t; */
