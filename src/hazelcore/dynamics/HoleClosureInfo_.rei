/* HoleClosureInfo_.re: Auxiliary data structure for constructing a
   HoleClosureInfo.t.

   Useful for building the HoleClosureInfo, because we can index/lookup
   by EnvironmentId. However, when using it we want sequential numbers
   (HoleClosureId) to identify the hole closures
   (similar to HoleInstanceInfo.t).
   */
[@deriving sexp]
type t =
  MetaVarMap.t(
    EnvironmentIdMap.t(
      (HoleClosureId.t, ClosureEnvironment.t, HoleClosureParents.t),
    ),
  );

let empty: t;

/* Assign a unique hole closure ID for the (u, env) pair representing
   a hole closure. If the pair already exists, return the existing value;
   otherwise, assign a new ID to the pair. */
let number_hole_closure:
  (t, MetaVar.t, ClosureEnvironment.t) => (t, HoleClosureId.t);

/* Converts HoleClosureInfo_.t to HoleClosureInfo.t */
let to_hole_closure_info: t => HoleClosureInfo.t;
