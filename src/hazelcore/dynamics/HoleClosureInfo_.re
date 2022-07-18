[@deriving sexp]
type t =
  MetaVarMap.t(
    EnvironmentIdMap.t((HoleClosureId.t, EvalEnv.t, HoleClosureParents.t)),
  );

let empty: t = MetaVarMap.empty;

let number_hole_closure =
    (hci: t, u: MetaVar.t, env: EvalEnv.t): (t, HoleClosureId.t) => {
  let ei = env |> EvalEnv.id_of;
  switch (hci |> MetaVarMap.find_opt(u)) {
  /* Hole already exists in the HoleClosureInfo_.t */
  | Some(hcs) =>
    switch (hcs |> EnvironmentIdMap.find_opt(ei)) {
    /* Hole closure already exists in the HoleClosureInfo_.t, simply
       return the hole closure number */
    | Some((i, _, _)) => (hci, i)
    /* Hole exists in the HoleClosureInfo_.t but closure doesn't.
       Create a new hole closure with closure id equal to the number
       of unique hole closures for the hole. Return a None environment */
    | None =>
      let i = hcs |> EnvironmentIdMap.cardinal;
      (
        hci
        |> MetaVarMap.add(u, hcs |> EnvironmentIdMap.add(ei, (i, env, []))),
        i,
      );
    }
  /* Hole doesn't exist in the HoleClosureInfo_.t */
  | None => (
      hci |> MetaVarMap.add(u, EnvironmentIdMap.singleton(ei, (0, env, []))),
      0,
    )
  };
};

let to_hole_closure_info = (hci: t): HoleClosureInfo.t =>
  /* For each hole, arrange closures in order of increasing hole
     closure id. */
  hci
  |> MetaVarMap.map(
       (
         hcs:
           EnvironmentIdMap.t(
             (HoleClosureId.t, EvalEnv.t, HoleClosureParents.t),
           ),
       ) =>
       hcs
       |> EnvironmentIdMap.bindings
       |> List.sort(((_, (i1, _, _)), (_, (i2, _, _))) =>
            compare(i1, i2)
          )
       |> List.map(((_, (_, sigma, hc_parents))) => (sigma, hc_parents))
     );
