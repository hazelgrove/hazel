type error_status =
  | Occurs;

type t = UnionFind.elem((mut_eq_typs, option(error_status)))
and mut_eq_typs = list(mut_eq_typ)
and mut_eq_typ =
  | Base(EqClass.base_typ)
  | Mapped(EqClass.unary_ctor, t)
  | Compound(EqClass.binary_ctor, t, t);

let wrap_without_error = (typs: mut_eq_typs): t => {
  (typs, None) |> UnionFind.make;
};

let unwrap_and_remove_error = (t: t): mut_eq_typs => {
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

let rec snapshot_class =
        (mut_eq_class: t, occurs_rep: ITyp.t)
        : (EqClass.t, option(error_status)) => {
  let (typs, err1) = UnionFind.get(mut_eq_class);
  let (eq_class, err2) = snapshot_typs(typs, mut_eq_class, occurs_rep);
  (eq_class, combine_error_status(err1, err2));
}
and snapshot_class_from_child =
    (mut_eq_class: t, parent: t, occurs_rep: ITyp.t)
    : (EqClass.t, option(error_status)) => {
  UnionFind.eq(mut_eq_class, parent)
    ? ([occurs_rep |> EqClass.ityp_to_eq_typ], Some(Occurs))
    : snapshot_class(mut_eq_class, occurs_rep);
}
and snapshot_typs =
    (mut_eq_typs: mut_eq_typs, parent: t, occurs_rep: ITyp.t)
    : (EqClass.t, option(error_status)) => {
  switch (mut_eq_typs) {
  | [] => ([], None)
  | [hd, ...tl] =>
    let (eq_typ_hd, err_hd) = snapshot_typ(hd, parent, occurs_rep);
    let (eq_class_tl, err_tl) = snapshot_typs(tl, parent, occurs_rep);
    ([eq_typ_hd, ...eq_class_tl], combine_error_status(err_hd, err_tl));
  };
}
and snapshot_typ =
    (mut_eq_typ: mut_eq_typ, parent: t, occurs_rep: ITyp.t)
    : (EqClass.eq_typ, option(error_status)) => {
  switch (mut_eq_typ) {
  | Base(b) => (EqClass.Base(b), None)
  | Compound(ctor, mut_eq_class_lhs, mut_eq_class_rhs) =>
    let (eq_class_lhs, err_lhs) =
      snapshot_class_from_child(mut_eq_class_lhs, parent, occurs_rep);
    let (eq_class_rhs, err_rhs) =
      snapshot_class_from_child(mut_eq_class_rhs, parent, occurs_rep);
    (
      EqClass.Compound(ctor, eq_class_lhs, eq_class_rhs),
      combine_error_status(err_lhs, err_rhs),
    );
  | Mapped(ctor, mut_eq_class) =>
    let (eq_class, err) =
      snapshot_class_from_child(mut_eq_class, parent, occurs_rep);
    (EqClass.Mapped(ctor, eq_class), err);
  };
};

let rec eq_class_to_mut_eq_class = (eq_class: EqClass.t): t => {
  List.map(eq_typ_to_mut_eq_typ, eq_class) |> wrap_without_error;
}
and eq_typ_to_mut_eq_typ = (eq_typ: EqClass.eq_typ): mut_eq_typ => {
  switch (eq_typ) {
  | Base(base_typ) => Base(base_typ)
  | Mapped(ctor, eq_class) =>
    Mapped(ctor, eq_class_to_mut_eq_class(eq_class))
  | Compound(ctor, eq_class_lhs, eq_class_rhs) =>
    Compound(
      ctor,
      eq_class_to_mut_eq_class(eq_class_lhs),
      eq_class_to_mut_eq_class(eq_class_rhs),
    )
  };
};

let rec preorder_elem_traversal_mut_eq_class = (mut_eq_class: t): list(t) => {
  [
    mut_eq_class,
    ...mut_eq_class
       |> unwrap_and_remove_error
       |> List.map(preorder_traversal_mut_eq_typ)
       |> List.flatten,
  ];
}
and preorder_traversal_mut_eq_typ = (mut_eq_typ: mut_eq_typ): list(t) => {
  switch (mut_eq_typ) {
  | Base(_) => []
  | Mapped(_, eq_class) => preorder_elem_traversal_mut_eq_class(eq_class)
  | Compound(_, lhs, rhs) =>
    preorder_elem_traversal_mut_eq_class(lhs)
    @ preorder_elem_traversal_mut_eq_class(rhs)
  };
};

let rec preorder_key_traversal_typ = (ty: ITyp.t): list(ITyp.t) => {
  switch (ty) {
  | Int
  | Unit
  | Float
  | String
  | Bool
  | Unknown(_) => [ty]
  | Arrow(ty_lhs, ty_rhs)
  | Prod(ty_lhs, ty_rhs)
  | Sum(ty_lhs, ty_rhs) => [
      ty,
      ...preorder_key_traversal_typ(ty_lhs)
         @ preorder_key_traversal_typ(ty_rhs),
    ]
  | List(list_ty) => [ty, ...preorder_key_traversal_typ(list_ty)]
  };
};

let derive_nested_keys_and_eq_classes =
    (key: ITyp.t): (list(ITyp.t), list(t)) => {
  let mut_eq_class =
    [key |> EqClass.ityp_to_eq_typ] |> eq_class_to_mut_eq_class;

  let preorder_typs = preorder_key_traversal_typ(key);
  let preorder_elems = preorder_elem_traversal_mut_eq_class(mut_eq_class);

  List.combine(preorder_typs, preorder_elems)
  |> List.filter(((k, _)) => ITyp.contains_hole(k))
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
    (target: mut_eq_typs, extension: mut_eq_typs): mut_eq_typs => {
  switch (extension) {
  | [] => target
  | [eq_typ_extension, ...extension_tl] =>
    let target = extend_typs_with_typ(target, eq_typ_extension);
    extend_typs_with_typs(target, extension_tl);
  };
}
and extend_typs_with_typ =
    (target: mut_eq_typs, eq_typ_extension: mut_eq_typ): mut_eq_typs => {
  switch (target) {
  | [] => [eq_typ_extension]
  | [target_hd, ...target_tl] =>
    let extend_target_tl: unit => mut_eq_typs = (
      () => {
        [target_hd, ...extend_typs_with_typ(target_tl, eq_typ_extension)];
      }
    );
    switch (target_hd, eq_typ_extension) {
    | (_, Base(_)) =>
      target_hd == eq_typ_extension ? target : extend_target_tl()
    | (Mapped(hd_ctor, hd_eq_class), Mapped(eq_typ_ctor, eq_class)) =>
      hd_ctor == eq_typ_ctor
        ? [
          Mapped(hd_ctor, extend_class_with_class(hd_eq_class, eq_class)),
          ...target_tl,
        ]
        : extend_target_tl()
    | (
        Compound(hd_ctor, hd_eq_class_lt, hd_eq_class_rt),
        Compound(eq_typ_ctor, eq_class_lt, eq_class_rt),
      ) =>
      if (hd_ctor == eq_typ_ctor) {
        let hd_eq_class_lt =
          extend_class_with_class(hd_eq_class_lt, eq_class_lt);
        let hd_eq_class_rt =
          extend_class_with_class(hd_eq_class_rt, eq_class_rt);
        [Compound(hd_ctor, hd_eq_class_lt, hd_eq_class_rt), ...target_tl];
      } else {
        extend_target_tl();
      }
    | (Base(_) | Mapped(_), Compound(_))
    | (Base(_) | Compound(_), Mapped(_)) => extend_target_tl()
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

let mark_failed_occurs = (mut_eq_class: t): unit => {
  let (curr_typs, _) = UnionFind.get(mut_eq_class);
  UnionFind.set(mut_eq_class, (curr_typs, Some(Occurs)));
};
