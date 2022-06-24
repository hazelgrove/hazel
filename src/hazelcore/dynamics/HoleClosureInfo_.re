/* HoleClosureInfo_.re: Auxiliary data structure for constructing a
   HoleClosureInfo.t.

   Useful for building the HoleClosureInfo, because we can index/lookup
   by EvalEnvId. However, when using it we want sequential numbers
   (HoleClosureId) to identify the hole closures
   (similar to HoleInstanceInfo.t).
   */
[@deriving sexp]
type t =
  MetaVarMap.t(
    EvalEnvIdMap.t((HoleClosureId.t, EvalEnv.t, HoleClosureParents.t)),
  );

let empty: t = MetaVarMap.empty;

/* Assign a unique hole closure ID for the (u, env) pair representing
   a hole closure. If the pair already exists, return the existing value;
   otherwise, assign a new ID to the pair. */
let number_hole_closure =
    (hci: t, u: MetaVar.t, env: EvalEnv.t): (t, HoleClosureId.t) => {
  let ei = env |> EvalEnv.id_of_evalenv;
  switch (hci |> MetaVarMap.find_opt(u)) {
  /* Hole already exists in the HoleClosureInfo_.t */
  | Some(hcs) =>
    switch (hcs |> EvalEnvIdMap.find_opt(ei)) {
    /* Hole closure already exists in the HoleClosureInfo_.t, simply
       return the hole closure number */
    | Some((i, _, _)) => (hci, i)
    /* Hole exists in the HoleClosureInfo_.t but closure doesn't.
       Create a new hole closure with closure id equal to the number
       of unique hole closures for the hole. Return a None environment */
    | None =>
      let i = hcs |> EvalEnvIdMap.cardinal;
      (
        hci |> MetaVarMap.add(u, hcs |> EvalEnvIdMap.add(ei, (i, env, []))),
        i,
      );
    }
  /* Hole doesn't exist in the HoleClosureInfo_.t */
  | None => (
      hci |> MetaVarMap.add(u, EvalEnvIdMap.singleton(ei, (0, env, []))),
      0,
    )
  };
};

/* Converts HoleClosureInfo_.t to HoleClosureInfo.t
 */
let to_hole_closure_info = (hci: t): HoleClosureInfo.t =>
  /* For each hole, arrange closures in order of increasing hole
     closure id. */
  hci
  |> MetaVarMap.map(
       (
         hcs:
           EvalEnvIdMap.t(
             (HoleClosureId.t, EvalEnv.t, HoleClosureParents.t),
           ),
       ) =>
       hcs
       |> EvalEnvIdMap.bindings
       |> List.sort(((_, (i1, _, _)), (_, (i2, _, _))) =>
            compare(i1, i2)
          )
       |> List.map(((_, (_, sigma, hc_parents))) => (sigma, hc_parents))
     );
