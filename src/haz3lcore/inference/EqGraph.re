type t = Hashtbl.t(ITyp.t, MutableEqClass.t);

let expected_size: int = 50;

let create = (): t => {
  Hashtbl.create(expected_size);
};

let add = (eq_graph: t, key: ITyp.t, mut_eq_class: MutableEqClass.t): unit => {
  switch (Hashtbl.find_opt(eq_graph, key)) {
  | Some(curr_mut_eq_class) =>
    MutableEqClass.union(curr_mut_eq_class, mut_eq_class)
  | None => Hashtbl.add(eq_graph, key, mut_eq_class)
  };
};

let add_typ_as_node = (eq_graph: t, typ: ITyp.t): unit => {
  let (keys, values) = MutableEqClass.derive_nested_keys_and_eq_classes(typ);
  List.iter2(add(eq_graph), keys, values);
};

let equate_nodes = (eq_graph: t, typ1: ITyp.t, typ2: ITyp.t): unit => {
  let elem1 = Hashtbl.find(eq_graph, typ1);
  let elem2 = Hashtbl.find(eq_graph, typ2);

  MutableEqClass.union(elem1, elem2);
};

let equate_node_to_primitive_typ =
    (eq_graph: t, node_key: ITyp.t, equated_typ: ITyp.t): unit => {
  let curr_eq_class = Hashtbl.find(eq_graph, node_key);
  let mut_eq_typs_extension =
    [equated_typ |> EqClass.ityp_to_eq_typ]
    |> MutableEqClass.eq_class_to_mut_eq_class;

  MutableEqClass.union(curr_eq_class, mut_eq_typs_extension);
};

let get_keys_in_eq_class = (eq_graph: t, eq_class: EqClass.t): list(ITyp.t) => {
  let add_key_to_acc = (key: ITyp.t, _: MutableEqClass.t, acc: list(ITyp.t)) => {
    [key, ...acc];
  };
  let keys = Hashtbl.fold(add_key_to_acc, eq_graph, []);
  let is_in_eq_class = (key: ITyp.t) => {
    let key_eq_typ = EqClass.ityp_to_eq_typ(key);
    EqClass.target_typ_is_in_eq_class(key_eq_typ, eq_class);
  };
  List.filter(is_in_eq_class, keys);
};

let fail_occurs_check = (eq_graph: t, t1: ITyp.t, t2: ITyp.t): bool => {
  let c1 = Hashtbl.find(eq_graph, t1);
  let c2 = Hashtbl.find(eq_graph, t2);

  let (snapshot1, err1) = MutableEqClass.snapshot_class(c1);
  let (snapshot2, err2) = MutableEqClass.snapshot_class(c2);

  switch (err1, err2) {
  | (Some(MutableEqClass.Occurs), _)
  | (_, Some(MutableEqClass.Occurs)) => true
  | _ =>
    let keys_in_snapshot1 = get_keys_in_eq_class(eq_graph, snapshot1);
    let keys_in_snapshot2 = get_keys_in_eq_class(eq_graph, snapshot2);

    List.exists(
      EqClass.target_typ_in_domain_but_not_equal(snapshot1),
      List.map(EqClass.ityp_to_eq_typ, keys_in_snapshot2),
    )
    || List.exists(
         EqClass.target_typ_in_domain_but_not_equal(snapshot2),
         List.map(EqClass.ityp_to_eq_typ, keys_in_snapshot1),
       );
  };
};

let make_occurs_check = (eq_graph: t, t1: ITyp.t, t2: ITyp.t): unit =>
  if (fail_occurs_check(eq_graph, t1, t2)) {
    let elem1 = Hashtbl.find(eq_graph, t1);
    let elem2 = Hashtbl.find(eq_graph, t2);

    MutableEqClass.mark_failed_occurs(elem1);
    MutableEqClass.mark_failed_occurs(elem2);
  };
