[@deriving sexp]
type result = EvalEnv.result;

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

/* TODO: annotate */
module HoleClosureCtx: {
  [@deriving sexp]
  type t = MetaVarMap.t(IntMap.t((int, EvalEnv.t)));
  let empty: t;
  let mem_hole_closure:
    (t, MetaVar.t, EvalEnv.t) => option((int, EvalEnv.t));
  let get_hole_closure_id: (t, MetaVar.t, EvalEnv.t) => (t, int);
};

let expand_closures_to_lambdas:
  (HoleClosureCtx.t, DHExp.t) => (HoleClosureCtx.t, DHExp.t);

let evaluate:
  (EvalEnv.EvalEnvCtx.t, EvalEnv.t, DHExp.t) => (EvalEnv.EvalEnvCtx.t, result);

/* closed substitution [d1/x]d2;
   Not needed for evaluation with environments,
   leaving in case it's useful for something else */
/* let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;
   let subst: (Environment.t, DHExp.t) => DHExp.t; */
