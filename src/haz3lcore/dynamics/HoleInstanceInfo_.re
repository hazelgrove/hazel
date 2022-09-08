/*
  Variable names:
    [hii] => "hole instance info"
    [his] => "hole instances"
    [hip] => "hole instance parents"

  TODO: Clear explanation of namings, probably in overall doc.
 */

/**
  Map associates a hole id to a hole instance id, hole closure environment, and
  hole instance parents.
 */
[@deriving sexp]
type t =
  MetaVarMap.t(
    EnvironmentIdMap.t(
      (HoleInstanceId.t, ClosureEnvironment.t, HoleInstanceParents.t),
    ),
  );

let empty: t = MetaVarMap.empty;

let add_instance =
    (hii: t, u: MetaVar.t, env: ClosureEnvironment.t): (t, HoleInstanceId.t) => {
  let ei = env |> ClosureEnvironment.id_of;
  switch (hii |> MetaVarMap.find_opt(u)) {
  /* Hole already exists in the map. */
  | Some(his) =>
    switch (his |> EnvironmentIdMap.find_opt(ei)) {
    /* Hole instance already exists in the map, simply return the hole instance
     * id. */
    | Some((i, _, _)) => (hii, i)
    /* Hole exists in the info map, but instance doesn't; create a new hole
     * instance with next unique instance id. */
    | None =>
      let i = his |> EnvironmentIdMap.cardinal;
      let his = his |> EnvironmentIdMap.add(ei, (i, env, []));
      let hii = hii |> MetaVarMap.add(u, his);
      (hii, i);
    }
  /* Hole doesn't exist in the map. */
  | None =>
    let i = 0;
    let his = EnvironmentIdMap.singleton(ei, (0, env, []));
    let hii = hii |> MetaVarMap.add(u, his);
    (hii, i);
  };
};

let to_hole_instance_info = (hii: t): HoleInstanceInfo.t =>
  /* For each hole, arrange instances in order of increasing hole instance id. */
  hii
  |> MetaVarMap.map(his =>
       his
       |> EnvironmentIdMap.bindings
       |> List.sort(((_, (i1, _, _)), (_, (i2, _, _))) =>
            compare(i1, i2)
          )
       |> List.map(((_, (_, env, hip))) => (env, hip))
     );
