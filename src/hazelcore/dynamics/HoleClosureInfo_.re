[@deriving sexp]
type t = MetaVarMap.t(EvalEnvIdMap.t(HoleClosure.t));

let empty: t = MetaVarMap.empty;

let find_hc_opt =
    (hci: t, u: MetaVar.t, sigma: EvalEnv.t): option(HoleClosure.t) => {
  let ei =
    sigma
    |> EvalEnv.id_of_evalenv
    |> OptUtil.get(_ => raise(EvalEnv.InvalidEvalEnvType));
  switch (hci |> MetaVarMap.find_opt(u)) {
  | Some(hcs) => hcs |> EvalEnvIdMap.find_opt(ei)
  | None => None
  };
};

let add_hc = (hci: t, u: MetaVar.t, sigma: EvalEnv.t): (t, HoleClosureId.t) => {
  let ei =
    sigma
    |> EvalEnv.id_of_evalenv
    |> OptUtil.get(_ => raise(EvalEnv.InvalidEvalEnvType));
  switch (hci |> MetaVarMap.find_opt(u)) {
  /* Hole already exists in the HoleClosureInfo_.t */
  | Some(hcs) =>
    switch (hcs |> EvalEnvIdMap.find_opt(ei)) {
    /* Hole closure already exists in the HoleClosureInfo_.t */
    | Some((_, i, _)) => (hci, i)
    /* Hole exists in the HoleClosureInfo_.t but closure doesn't.
       Create a new hole closure with closure id equal to the number
       of unique hole closures for the hole. */
    | None =>
      let i = hcs |> EvalEnvIdMap.cardinal;
      (
        hci |> MetaVarMap.add(u, hcs |> EvalEnvIdMap.add(ei, (u, i, sigma))),
        i,
      );
    }
  /* Hole already exists in the HoleClosureInfo_.t */
  | None => (
      hci |> MetaVarMap.add(u, EvalEnvIdMap.singleton(ei, (u, 0, sigma))),
      0,
    )
  };
};

let to_hole_closure_info = (hci: t): HoleClosureInfo.t =>
  /* For each hole, arrange closures in order of increasing hole
     closure id. */
  hci
  |> MetaVarMap.map((hcs: EvalEnvIdMap.t(HoleClosure.t)) =>
       hcs
       |> EvalEnvIdMap.bindings
       |> List.map(((_, hc)) => hc)
       |> List.sort(((_, i1, _), (_, i2, _)) => compare(i1, i2))
     );
