/* Auxiliary data structure for constructing a
   HoleClosureInfo.t. Useful for building the HoleClosureInfo,
   because we can index/lookup by EvalEnvId. However,
   when using it we want sequential numbers (HoleClosureId)
   to identify the hole closures (similar to HoleInstanceInfo.t).
   */
[@deriving sexp]
type t = MetaVarMap.t(EvalEnvIdMap.t((HoleClosureId.t, EvalEnv.t)));

let empty: t;

/* Returns the HoleClosure.t with the given u and sigma, if found */
let find_hc_opt:
  (t, MetaVar.t, EvalEnv.t) => option((HoleClosureId.t, EvalEnv.t));

/* Installs a hole closure and returns the HoleClosureId.t. If the
   hole closure already exists in the HoleClosureInfo_.t,
   thsi returns the HoleClosureInfo_.t unmodified and the
   closure's HoleClosureId.t */
let add_hc: (t, MetaVar.t, EvalEnv.t) => (t, HoleClosureId.t);

/* Converts HoleClosureInfo_.t to HoleClosureInfo.t */
let to_hole_closure_info: t => HoleClosureInfo.t;
