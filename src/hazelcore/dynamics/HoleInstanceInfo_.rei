/* HoleInstanceInfo_.re: Auxiliary data structure for constructing a
   HoleInstanceInfo.t.

   Useful for building the HoleInstanceInfo, because we can index/lookup
   by EnvironmentId. However, when using it we want sequential numbers
   (HoleInstanceId) to identify the hole closures
   (similar to HoleInstanceInfo.t).
   */
[@deriving sexp]
type t =
  MetaVarMap.t(
    EnvironmentIdMap.t(
      (HoleInstanceId.t, ClosureEnvironment.t, HoleInstanceParents.t),
    ),
  );

let empty: t;

/* Assign a unique hole closure ID for the (u, env) pair representing
   a hole closure. If the pair already exists, return the existing value;
   otherwise, assign a new ID to the pair. */
let number_hole_closure:
  (t, MetaVar.t, ClosureEnvironment.t) => (t, HoleInstanceId.t);

/* Converts HoleInstanceInfo_.t to HoleInstanceInfo.t */
let to_hole_closure_info: t => HoleInstanceInfo.t;
