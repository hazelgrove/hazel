type error_status =
  | Occurs;

type t = UnionFind.elem((mut_pot_typs, option(error_status)))
and mut_pot_typs = list(mut_pot_typ)
and mut_pot_typ =
  | Base(PotentialTypeSet.base_typ)
  | Unary(PotentialTypeSet.unary_ctor, t)
  | Binary(PotentialTypeSet.binary_ctor, t, t);

let wrap_without_error = (typs: mut_pot_typs): t => {
  (typs, None) |> UnionFind.make;
};

let unwrap_and_remove_error = (t: t): mut_pot_typs => {
  let (typs, _) = UnionFind.get(t);
  typs;
};

let combine_error_status =
    (err1: option(error_status), err2: option(error_status)) => {
  switch (err1, err2) {
  | (None, None) => None
  | (Some(Occurs), Some(Occurs))
  | (Some(Occurs), None)
  | (None, Some(Occurs)) => Some(Occurs)
  };
};

let get_combined_error_status_of_classes =
    (t1: t, t2: t): option(error_status) => {
  let (_, err1) = UnionFind.get(t1);
  let (_, err2) = UnionFind.get(t2);

  combine_error_status(err1, err2);
};

let rec snapshot_class_wrapper =
        (mut_potential_typ_set: t, occurs_rep: ITyp.t, parents: list(t))
        : (PotentialTypeSet.t, option(error_status)) => {
  let (typs, err1) = UnionFind.get(mut_potential_typ_set);
  let (potential_typ_set, err2) =
    snapshot_typs(typs, [mut_potential_typ_set, ...parents], occurs_rep);
  (potential_typ_set, combine_error_status(err1, err2));
}
and snapshot_class_from_child =
    (mut_potential_typ_set: t, parents: list(t), occurs_rep: ITyp.t)
    : (PotentialTypeSet.t, option(error_status)) => {
  List.exists(UnionFind.eq(mut_potential_typ_set), parents)
    ? (
      [occurs_rep |> PotentialTypeSet.ityp_to_potential_typ],
      Some(Occurs),
    )
    : snapshot_class_wrapper(mut_potential_typ_set, occurs_rep, parents);
}
and snapshot_typs =
    (mut_pot_typs: mut_pot_typs, parents: list(t), occurs_rep: ITyp.t)
    : (PotentialTypeSet.t, option(error_status)) => {
  switch (mut_pot_typs) {
  | [] => ([], None)
  | [hd, ...tl] =>
    let (pot_typ_hd, err_hd) = snapshot_typ(hd, parents, occurs_rep);
    let (potential_typ_set_tl, err_tl) =
      snapshot_typs(tl, parents, occurs_rep);
    (
      [pot_typ_hd, ...potential_typ_set_tl],
      combine_error_status(err_hd, err_tl),
    );
  };
}
and snapshot_typ =
    (mut_pot_typ: mut_pot_typ, parents: list(t), occurs_rep: ITyp.t)
    : (PotentialTypeSet.potential_typ, option(error_status)) => {
  switch (mut_pot_typ) {
  | Base(b) => (PotentialTypeSet.Base(b), None)
  | Binary(ctor, mut_potential_typ_set_lhs, mut_potential_typ_set_rhs) =>
    let (potential_typ_set_lhs, err_lhs) =
      snapshot_class_from_child(
        mut_potential_typ_set_lhs,
        parents,
        occurs_rep,
      );
    let (potential_typ_set_rhs, err_rhs) =
      snapshot_class_from_child(
        mut_potential_typ_set_rhs,
        parents,
        occurs_rep,
      );
    (
      PotentialTypeSet.Binary(
        ctor,
        potential_typ_set_lhs,
        potential_typ_set_rhs,
      ),
      combine_error_status(err_lhs, err_rhs),
    );
  | Unary(ctor, mut_potential_typ_set) =>
    let (potential_typ_set, err) =
      snapshot_class_from_child(mut_potential_typ_set, parents, occurs_rep);
    (PotentialTypeSet.Unary(ctor, potential_typ_set), err);
  };
};

let snapshot_class = (mut_potential_typ_set: t, occurs_rep: ITyp.t) =>
  snapshot_class_wrapper(mut_potential_typ_set, occurs_rep, []);

let rec pot_typ_set_to_mut_pot_typ_set =
        (potential_typ_set: PotentialTypeSet.t): t => {
  List.map(pot_typ_to_mut_pot_typ, potential_typ_set) |> wrap_without_error;
}
and pot_typ_to_mut_pot_typ =
    (pot_typ: PotentialTypeSet.potential_typ): mut_pot_typ => {
  switch (pot_typ) {
  | Base(base_typ) => Base(base_typ)
  | Unary(ctor, potential_typ_set) =>
    Unary(ctor, pot_typ_set_to_mut_pot_typ_set(potential_typ_set))
  | Binary(ctor, potential_typ_set_lhs, potential_typ_set_rhs) =>
    Binary(
      ctor,
      pot_typ_set_to_mut_pot_typ_set(potential_typ_set_lhs),
      pot_typ_set_to_mut_pot_typ_set(potential_typ_set_rhs),
    )
  };
};

let rec preorder_elem_traversal_mut_potential_typ_set =
        (mut_potential_typ_set: t): list(t) => {
  [
    mut_potential_typ_set,
    ...mut_potential_typ_set
       |> unwrap_and_remove_error
       |> List.map(preorder_traversal_mut_pot_typ)
       |> List.flatten,
  ];
}
and preorder_traversal_mut_pot_typ = (mut_pot_typ: mut_pot_typ): list(t) => {
  switch (mut_pot_typ) {
  | Base(_) => []
  | Unary(_, potential_typ_set) =>
    preorder_elem_traversal_mut_potential_typ_set(potential_typ_set)
  | Binary(_, lhs, rhs) =>
    preorder_elem_traversal_mut_potential_typ_set(lhs)
    @ preorder_elem_traversal_mut_potential_typ_set(rhs)
  };
};

let rec preorder_key_traversal_typ = (ty: ITyp.t): list(ITyp.t) => {
  switch (ty) {
  | Int
  | Unit
  | Float
  | String
  | Bool
  | Var(_)
  | Unknown(_) => [ty]
  | Arrow(ty_lhs, ty_rhs)
  | Prod(ty_lhs, ty_rhs)
  | Sum(ty_lhs, ty_rhs) => [
      ty,
      ...preorder_key_traversal_typ(ty_lhs)
         @ preorder_key_traversal_typ(ty_rhs),
    ]
  | List(list_ty) => [ty, ...preorder_key_traversal_typ(list_ty)]
  | Named(_, _, named_ty) => [ty, ...preorder_key_traversal_typ(named_ty)]
  };
};

let derive_nested_keys_and_potential_typ_sets =
    (key: ITyp.t): (list(ITyp.t), list(t)) => {
  let mut_potential_typ_set =
    [key |> PotentialTypeSet.ityp_to_potential_typ]
    |> pot_typ_set_to_mut_pot_typ_set;

  let preorder_typs = preorder_key_traversal_typ(key);
  let preorder_elems =
    preorder_elem_traversal_mut_potential_typ_set(mut_potential_typ_set);

  List.combine(preorder_typs, preorder_elems)
  |> List.filter(((k, _)) => ITyp.contains_node(k))
  |> List.split;
};

let rec extend_class_with_class = (target: t, extension: t): t => {
  let merged_typs =
    extend_typs_with_typs(
      unwrap_and_remove_error(target),
      unwrap_and_remove_error(extension),
    );
  let final_rep = UnionFind.union(target, extension);
  UnionFind.set(
    final_rep,
    (merged_typs, get_combined_error_status_of_classes(target, extension)),
  );
  final_rep;
}
and extend_typs_with_typs =
    (target: mut_pot_typs, extension: mut_pot_typs): mut_pot_typs => {
  switch (extension) {
  | [] => target
  | [pot_typ_extension, ...extension_tl] =>
    let target = extend_typs_with_typ(target, pot_typ_extension);
    extend_typs_with_typs(target, extension_tl);
  };
}
and extend_typs_with_typ =
    (target: mut_pot_typs, pot_typ_extension: mut_pot_typ): mut_pot_typs => {
  switch (target) {
  | [] => [pot_typ_extension]
  | [target_hd, ...target_tl] =>
    let extend_target_tl: unit => mut_pot_typs = (
      () => {
        [target_hd, ...extend_typs_with_typ(target_tl, pot_typ_extension)];
      }
    );
    switch (target_hd, pot_typ_extension) {
    | (_, Base(_)) =>
      target_hd == pot_typ_extension ? target : extend_target_tl()
    | (
        Unary(hd_ctor, hd_potential_typ_set),
        Unary(pot_typ_ctor, potential_typ_set),
      ) =>
      hd_ctor == pot_typ_ctor
        ? [
          Unary(
            hd_ctor,
            extend_class_with_class(hd_potential_typ_set, potential_typ_set),
          ),
          ...target_tl,
        ]
        : extend_target_tl()
    | (
        Binary(hd_ctor, hd_potential_typ_set_lt, hd_potential_typ_set_rt),
        Binary(pot_typ_ctor, potential_typ_set_lt, potential_typ_set_rt),
      ) =>
      if (hd_ctor == pot_typ_ctor) {
        let hd_potential_typ_set_lt =
          extend_class_with_class(
            hd_potential_typ_set_lt,
            potential_typ_set_lt,
          );
        let hd_potential_typ_set_rt =
          extend_class_with_class(
            hd_potential_typ_set_rt,
            potential_typ_set_rt,
          );
        [
          Binary(hd_ctor, hd_potential_typ_set_lt, hd_potential_typ_set_rt),
          ...target_tl,
        ];
      } else {
        extend_target_tl();
      }
    | (Base(_) | Unary(_), Binary(_))
    | (Base(_) | Binary(_), Unary(_)) => extend_target_tl()
    };
  };
};

let union = (t1: t, t2: t): unit =>
  if (UnionFind.eq(t1, t2)) {
    ();
  } else {
    let _ = extend_class_with_class(t1, t2);
    ();
  };

let mark_failed_occurs = (mut_potential_typ_set: t): unit => {
  let (curr_typs, _) = UnionFind.get(mut_potential_typ_set);
  UnionFind.set(mut_potential_typ_set, (curr_typs, Some(Occurs)));
};
