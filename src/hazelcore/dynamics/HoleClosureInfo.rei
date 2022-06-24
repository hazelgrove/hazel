/* Stores information about all hole closures reachable
   by a program's evaluation result. Used in the context
   inspector.

   Constructed using HoleClosureInfo_.t. */
[@deriving sexp]
type t = MetaVarMap.t(list((EvalEnv.t, HoleClosureParents.t)));

let empty: t;

/* Number of unique closures for a given hole. */
let num_unique_hcs: (t, MetaVar.t) => int;

/* Returns the information for a given hole and hole closure
   id, if found. */
let find_hc_opt:
  (t, MetaVar.t, HoleClosureId.t) =>
  option((EvalEnv.t, HoleClosureParents.t));

let add_parent: (HoleClosure.t, HoleClosureParents.t_, t) => t;
