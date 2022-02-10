[@deriving sexp]
type t =
  MetaVarMap.t(
    EvalEnvIdMap.t((HoleClosureId.t, EvalEnv.t, HoleClosureParents.t)),
  );

let empty = MetaVarMap.empty;

type hc_id_result =
  | ExistClosure(t, HoleClosureId.t, EvalEnv.t)
  | NewClosure(t, HoleClosureId.t);
let get_hc_id =
    (hci: t, u: MetaVar.t, sigma: EvalEnv.t, parent: HoleClosureParents.t_)
    : hc_id_result => {
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
    | Some((i, sigma, hole_parents)) =>
      ExistClosure(
        hci
        |> MetaVarMap.add(
             u,
             hcs
             |> EvalEnvIdMap.add(
                  ei,
                  (
                    i,
                    sigma,
                    parent |> HoleClosureParents.add_parent(hole_parents),
                  ),
                ),
           ),
        i,
        sigma,
      )
    /* Hole exists in the HoleClosureInfo_.t but closure doesn't.
       Create a new hole closure with closure id equal to the number
       of unique hole closures for the hole. Return a None environment */
    | None =>
      let i = hcs |> EvalEnvIdMap.cardinal;
      NewClosure(
        hci
        |> MetaVarMap.add(
             u,
             hcs
             |> EvalEnvIdMap.add(
                  ei,
                  (i, sigma, parent |> HoleClosureParents.singleton),
                ),
           ),
        i,
      );
    }
  /* Hole doesn't exist in the HoleClosureInfo_.t */
  | None =>
    NewClosure(
      hci
      |> MetaVarMap.add(
           u,
           EvalEnvIdMap.singleton(
             ei,
             (0, sigma, parent |> HoleClosureParents.singleton),
           ),
         ),
      0,
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
              Option.map(((hcid, _, parents)) => (hcid, sigma, parents)),
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
