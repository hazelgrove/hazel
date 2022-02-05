[@deriving sexp]
type t =
  MetaVarMap.t(
    EvalEnvIdMap.t((HoleClosureId.t, EvalEnv.t, HoleClosureParents.t)),
  );

let empty = MetaVarMap.empty;

let get_hc_id =
    (hci: t, u: MetaVar.t, sigma: EvalEnv.t, parent_hc: HoleClosure.t)
    : (t, HoleClosureId.t, option(EvalEnv.t)) => {
  let ei =
    sigma
    |> EvalEnv.id_of_evalenv
    |> OptUtil.get(_ => raise(EvalEnv.InvalidEvalEnvType));
  switch (hci |> MetaVarMap.find_opt(u)) {
  /* Hole already exists in the HoleClosureInfo_.t */
  | Some(hcs) =>
    switch (hcs |> EvalEnvIdMap.find_opt(ei)) {
    /* Hole closure already exists in the HoleClosureInfo_.t.
       Add parent_eid to eids */
    | Some((i, sigma', hole_parents)) => (
        hci
        |> MetaVarMap.add(
             u,
             hcs
             |> EvalEnvIdMap.add(
                  ei,
                  (
                    i,
                    sigma,
                    parent_hc |> HoleClosureParents.add_parent(hole_parents),
                  ),
                ),
           ),
        i,
        Some(sigma'),
      )
    /* Hole exists in the HoleClosureInfo_.t but closure doesn't.
       Create a new hole closure with closure id equal to the number
       of unique hole closures for the hole. Return a None environment */
    | None =>
      let i = hcs |> EvalEnvIdMap.cardinal;
      (
        hci
        |> MetaVarMap.add(
             u,
             hcs
             |> EvalEnvIdMap.add(
                  ei,
                  (i, sigma, parent_hc |> HoleClosureParents.singleton),
                ),
           ),
        i,
        None,
      );
    }
  /* Hole doesn't exist in the HoleClosureInfo_.t */
  | None => (
      hci
      |> MetaVarMap.add(
           u,
           EvalEnvIdMap.singleton(
             ei,
             (0, sigma, parent_hc |> HoleClosureParents.singleton),
           ),
         ),
      0,
      None,
    )
  };
};

let update_hc_env = (hci: t, u: MetaVar.t, sigma: EvalEnv.t): t => {
  let ei =
    sigma
    |> EvalEnv.id_of_evalenv
    |> OptUtil.get(_ => raise(EvalEnv.InvalidEvalEnvType));
  hci
  |> MetaVarMap.update(
       u,
       Option.map(hcs => {
         hcs
         |> EvalEnvIdMap.update(
              ei,
              Option.map(((hcid, _, parent_hcs)) =>
                (hcid, sigma, parent_hcs)
              ),
            )
       }),
     );
};

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
