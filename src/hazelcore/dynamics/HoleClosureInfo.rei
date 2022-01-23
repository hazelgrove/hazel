/* Stores information about all hole closures reachable
   by a program's evaluation result. Used in the context
   inspector.

   Constructed using HoleClosureInfo_.t. */
[@deriving sexp]
type t = MetaVarMap.t(list(HoleClosure.t));

let empty: t;

/* Number of unique closures for a given hole. */
let num_unique_hcs: (t, MetaVar.t) => int;

/* Returns the HoleClosure for a given hole and hole closure
   id, if found. */
let find_hc_opt: (t, MetaVar.t, HoleClosureId.t) => option(HoleClosure.t);
