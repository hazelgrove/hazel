/* Auxiliary data structure for constructing a
   HoleClosureInfo.t. Useful for building the HoleClosureInfo,
   because we can index/lookup by EvalEnvId. However,
   when using it we want sequential numbers (HoleClosureId)
   to identify the hole closures (similar to HoleInstanceInfo.t).
   */
[@deriving sexp]
type t =
  MetaVarMap.t(
    EvalEnvIdMap.t((HoleClosureId.t, EvalEnv.t, HoleClosureParents.t)),
  );

let empty: t;

/* The result type for function `get_hc_id`, which is similar to the
   `next` function in HoleInstanceInfo. If the given closure exists
   in type t, the result also includes the EvalEnv.t stored; otherwise,
   it's a new closure and no EvalEnv shall be returned.
   */
type hc_id_result =
  | ExistClosure(t, HoleClosureId.t, EvalEnv.t)
  | NewClosure(t, HoleClosureId.t);

/* Gets the hole closure id of a hole closure with the given
   hole number and hole environment. Also adds the current parent
   hole closure to the HoleClosureInfo_.t.

   If a hole closure with this hole number and environment already
   exists in the HoleClosureInfo_.t, then the parent hole closure is
   added to the HoleClosureInfo_.t, and the HoleClosureId.t and
   environment of the existing hole closure is returned.

   Otherwise, a new HoleClosureId.t is assigned to this hole closure,
   and is inserted into the HoleClosureId_.t. This is returned, along
   with a None EvalEnv.t.

   (similar to HoleInstanceInfo.next, but memoized by EvalEnvId.t)
   */
let get_hc_id: (t, MetaVar.t, EvalEnv.t, HoleClosure.t) => hc_id_result;

/* Updates the environment of the specified hole closure.

   (similar to HoleInstanceInfo.update_environment)
   */
let update_hc_env: (t, MetaVar.t, EvalEnv.t) => t;

/* Converts HoleClosureInfo_.t to HoleClosureInfo.t */
let to_hole_closure_info: t => HoleClosureInfo.t;
