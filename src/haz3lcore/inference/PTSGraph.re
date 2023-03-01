type t = Hashtbl.t(ITyp.t, MutablePotentialTypeSet.t);

let expected_size: int = 50;

let create = (): t => {
  Hashtbl.create(expected_size);
};

let add =
    (
      pts_graph: t,
      key: ITyp.t,
      mut_potential_type_set: MutablePotentialTypeSet.t,
    )
    : unit => {
  switch (Hashtbl.find_opt(pts_graph, key)) {
  | Some(curr_mut_potential_type_set) =>
    MutablePotentialTypeSet.union(
      curr_mut_potential_type_set,
      mut_potential_type_set,
    )
  | None => Hashtbl.add(pts_graph, key, mut_potential_type_set)
  };
};

let add_typ_as_node = (pts_graph: t, typ: ITyp.t): unit => {
  let (keys, values) =
    MutablePotentialTypeSet.derive_nested_keys_and_potential_typ_sets(typ);
  List.iter2(add(pts_graph), keys, values);
};

let create_traversable_edge = (pts_graph: t, typ1: ITyp.t, typ2: ITyp.t): unit => {
  let elem1 = Hashtbl.find(pts_graph, typ1);
  let elem2 = Hashtbl.find(pts_graph, typ2);

  MutablePotentialTypeSet.union(elem1, elem2);
};

let create_solution_edge =
    (pts_graph: t, node_key: ITyp.t, equated_typ: ITyp.t): unit => {
  let curr_potential_type_set = Hashtbl.find(pts_graph, node_key);
  let mut_potential_typs_extension =
    [equated_typ |> PotentialTypeSet.ityp_to_potential_typ]
    |> MutablePotentialTypeSet.pot_typ_set_to_mut_pot_typ_set;

  MutablePotentialTypeSet.union(
    curr_potential_type_set,
    mut_potential_typs_extension,
  );
};

let get_keys_in_potential_type_set =
    (pts_graph: t, potential_type_set: PotentialTypeSet.t): list(ITyp.t) => {
  let add_key_to_acc =
      (key: ITyp.t, _: MutablePotentialTypeSet.t, acc: list(ITyp.t)) => {
    [key, ...acc];
  };
  let keys = Hashtbl.fold(add_key_to_acc, pts_graph, []);
  let is_in_potential_type_set = (key: ITyp.t) => {
    let key_potential_typ = PotentialTypeSet.ityp_to_potential_typ(key);
    PotentialTypeSet.target_typ_is_in_potential_typ_set(
      key_potential_typ,
      potential_type_set,
    );
  };
  List.filter(is_in_potential_type_set, keys);
};

let fail_occurs_check = (pts_graph: t, t1: ITyp.t, t2: ITyp.t): bool => {
  let c1 = Hashtbl.find(pts_graph, t1);
  let c2 = Hashtbl.find(pts_graph, t2);

  let (snapshot1, err1) = MutablePotentialTypeSet.snapshot_class(c1, t1);
  let (snapshot2, err2) = MutablePotentialTypeSet.snapshot_class(c2, t2);

  switch (err1, err2) {
  | (Some(MutablePotentialTypeSet.Occurs), _)
  | (_, Some(MutablePotentialTypeSet.Occurs)) => true
  | _ =>
    let keys_in_snapshot1 =
      get_keys_in_potential_type_set(pts_graph, snapshot1);
    let keys_in_snapshot2 =
      get_keys_in_potential_type_set(pts_graph, snapshot2);

    List.exists(
      PotentialTypeSet.target_typ_in_domain_but_not_equal(snapshot1),
      List.map(PotentialTypeSet.ityp_to_potential_typ, keys_in_snapshot2),
    )
    || List.exists(
         PotentialTypeSet.target_typ_in_domain_but_not_equal(snapshot2),
         List.map(PotentialTypeSet.ityp_to_potential_typ, keys_in_snapshot1),
       );
  };
};

let make_occurs_check = (pts_graph: t, t1: ITyp.t, t2: ITyp.t): unit =>
  if (fail_occurs_check(pts_graph, t1, t2)) {
    let elem1 = Hashtbl.find(pts_graph, t1);
    let elem2 = Hashtbl.find(pts_graph, t2);

    MutablePotentialTypeSet.mark_failed_occurs(elem1);
    MutablePotentialTypeSet.mark_failed_occurs(elem2);
  };
