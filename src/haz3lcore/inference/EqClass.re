open Util;
open OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp)]
type base_typ =
  | BUnit
  | BInt
  | BFloat
  | BBool
  | BString
  | BUnknown(Typ.type_provenance);

[@deriving (show({with_path: false}), sexp)]
type unary_ctor =
  | CList;

[@deriving (show({with_path: false}), sexp)]
type binary_ctor =
  | CArrow
  | CProd
  | CSum;

[@deriving (show({with_path: false}), sexp)]
type t = list(eq_typ)
and eq_typ =
  | Base(base_typ)
  | Mapped(unary_ctor, t)
  | Compound(binary_ctor, t, t);

let mk_as_binary_ctor = (ctor: binary_ctor, ty1: ITyp.t, ty2: ITyp.t): ITyp.t => {
  switch (ctor) {
  | CArrow => Arrow(ty1, ty2)
  | CProd => Prod(ty1, ty2)
  | CSum => Sum(ty1, ty2)
  };
};

let mk_as_unary_ctor = (ctor: unary_ctor, ty: ITyp.t): ITyp.t => {
  switch (ctor) {
  | CList => List(ty)
  };
};

let rec ityp_to_eq_typ: ITyp.t => eq_typ =
  fun
  | Unknown(prov) => Base(BUnknown(prov))
  | Int => Base(BInt)
  | Unit => Base(BUnit)
  | Float => Base(BFloat)
  | Bool => Base(BBool)
  | String => Base(BString)
  | Arrow(ty1, ty2) =>
    Compound(CArrow, [ityp_to_eq_typ(ty1)], [ityp_to_eq_typ(ty2)])
  | Prod(ty1, ty2) =>
    Compound(CProd, [ityp_to_eq_typ(ty1)], [ityp_to_eq_typ(ty2)])
  | Sum(ty1, ty2) =>
    Compound(CProd, [ityp_to_eq_typ(ty1)], [ityp_to_eq_typ(ty2)])
  | List(ty) => Mapped(CList, [ityp_to_eq_typ(ty)]);

let typ_to_eq_typ: Typ.t => eq_typ =
  typ => {
    typ |> ITyp.typ_to_ityp |> ityp_to_eq_typ;
  };

let base_typ_to_ityp: base_typ => ITyp.t =
  fun
  | BInt => Int
  | BFloat => Float
  | BBool => Bool
  | BString => String
  | BUnit => Unit
  | BUnknown(prov) => Unknown(prov);

let rec extend_with_eq_class = (target: t, eq_class_extension: t) => {
  switch (eq_class_extension) {
  | [] => target
  | [eq_typ_extension, ...extension_tl] =>
    let target = extend_with_eq_typ(target, eq_typ_extension);
    extend_with_eq_class(target, extension_tl);
  };
}
and extend_with_eq_typ = (target: t, eq_typ_extension: eq_typ) => {
  switch (target) {
  | [] => [eq_typ_extension]
  | [target_hd, ...target_tl] =>
    let extend_target_tl: unit => t = (
      () => {
        [target_hd, ...extend_with_eq_typ(target_tl, eq_typ_extension)];
      }
    );
    switch (target_hd, eq_typ_extension) {
    | (_, Base(_)) =>
      target_hd == eq_typ_extension ? target : extend_target_tl()
    | (Mapped(hd_ctor, hd_eq_class), Mapped(eq_typ_ctor, eq_class)) =>
      hd_ctor == eq_typ_ctor
        ? [
          Mapped(hd_ctor, extend_with_eq_class(hd_eq_class, eq_class)),
          ...target_tl,
        ]
        : extend_target_tl()
    | (
        Compound(hd_ctor, hd_eq_class_lt, hd_eq_class_rt),
        Compound(eq_typ_ctor, eq_class_lt, eq_class_rt),
      ) =>
      if (hd_ctor == eq_typ_ctor) {
        let hd_eq_class_lt =
          extend_with_eq_class(hd_eq_class_lt, eq_class_lt);
        let hd_eq_class_rt =
          extend_with_eq_class(hd_eq_class_rt, eq_class_rt);
        [Compound(hd_ctor, hd_eq_class_lt, hd_eq_class_rt), ...target_tl];
      } else {
        extend_target_tl();
      }
    | (Base(_) | Mapped(_), Compound(_))
    | (Base(_) | Compound(_), Mapped(_)) => extend_target_tl()
    };
  };
};

type split_result =
  | Success
  | Error(split_error_status)
and split_error_status =
  | Unsplittable
  | WrongCtor;

let split_eq_typ: eq_typ => option((t, t)) =
  fun
  | Mapped(_)
  | Base(_) => None
  | Compound(_, eq_class1, eq_class2) => Some((eq_class1, eq_class2));

// not currently in use
let split_eq_class = (ctor_used: binary_ctor, eq_class: t) => {
  let split_result_of: eq_typ => split_result =
    fun
    | Base(ty) =>
      switch (ty) {
      | BUnknown(_) => Success
      | _ => Error(Unsplittable)
      }
    | Mapped(_) => Error(Unsplittable)
    | Compound(ctor, _, _) => ctor_used == ctor ? Success : Error(WrongCtor);

  let accumulate_splits =
      ((acc_class_lt, acc_class_rt): (t, t), eq_typ: eq_typ) => {
    switch (split_eq_typ(eq_typ)) {
    | None => (acc_class_lt, acc_class_rt)
    | Some((eq_class_lt, eq_class_rt)) =>
      let acc_class_lt = extend_with_eq_class(acc_class_lt, eq_class_lt);
      let acc_class_rt = extend_with_eq_class(acc_class_rt, eq_class_rt);
      (acc_class_lt, acc_class_rt);
    };
  };

  let (eq_class_lt, eq_class_rt) =
    List.fold_left(accumulate_splits, ([], []), eq_class);

  // Unsplittable errors take precedence over WrongCtor due to strictly more severe error handling
  let rec check_ctor =
          (eq_class: t, wrong_ctor_error_found: bool): split_result => {
    switch (eq_class) {
    | [] => wrong_ctor_error_found ? Error(WrongCtor) : Success
    | [hd, ...tl] =>
      switch (split_result_of(hd)) {
      | Error(Unsplittable) as e => e
      | Error(WrongCtor) => check_ctor(tl, true)
      | _ => check_ctor(tl, wrong_ctor_error_found)
      }
    };
  };

  (check_ctor(eq_class, false), eq_class_lt, eq_class_rt);
};

let fuse = (ctor_used: binary_ctor, eq_class_lt: t, eq_class_rt: t) => {
  Compound(ctor_used, eq_class_lt, eq_class_rt);
};

let rec target_typ_is_in_eq_class = (target_typ: eq_typ, eq_class: t): bool => {
  // is target_typ ∈ eq_class? this would make them equal (via transitivity)
  switch (eq_class) {
  | [] => false
  | [hd, ...tl] =>
    target_typ_is_in_eq_typ(target_typ, hd)
    || target_typ_is_in_eq_class(target_typ, tl)
  };
}
and target_typ_is_in_eq_typ = (target_typ: eq_typ, eq_typ: eq_typ): bool => {
  switch (target_typ, eq_typ) {
  | (_, Base(_)) => target_typ == eq_typ
  | (Mapped(target_ctor, target_eq_class), Mapped(ctor, eq_class)) =>
    target_ctor == ctor
    && target_class_is_in_eq_class(target_eq_class, eq_class)
  | (
      Compound(target_ctor, target_class_lt, target_class_rt),
      Compound(ctor, eq_class_lt, eq_class_rt),
    ) =>
    target_ctor == ctor
    && target_class_is_in_eq_class(target_class_lt, eq_class_lt)
    && target_class_is_in_eq_class(target_class_rt, eq_class_rt)
  | (Base(_) | Compound(_), Mapped(_))
  | (Base(_) | Mapped(_), Compound(_)) => false
  };
}
and target_class_is_in_eq_class = (target_class: t, eq_class: t): bool => {
  // is target_class ∈ eq_class? this would make them equal (via transitivity)
  let target_typ_contained = (target_typ: eq_typ): bool => {
    target_typ_is_in_eq_class(target_typ, eq_class);
  };
  List.for_all(target_typ_contained, target_class);
};

let rec target_typ_used_in_eq_class = (target_typ: eq_typ, eq_class: t): bool => {
  // is [target_typ] ⊆ eq_class?
  switch (eq_class) {
  | [] => false
  | [hd, ...tl] =>
    target_typ_used_in_eq_typ(target_typ, hd)
    || target_typ_used_in_eq_class(target_typ, tl)
  };
}
and target_typ_used_in_eq_typ = (target_typ: eq_typ, eq_typ: eq_typ): bool => {
  // target used inside, or is represented by the eq_typ itself
  switch (target_typ, eq_typ) {
  | (_, Base(_)) => target_typ == eq_typ
  | (Mapped(_), Mapped(_, eq_class)) =>
    target_typ_used_in_eq_class(target_typ, eq_class)
    || target_typ_is_in_eq_typ(target_typ, eq_typ)
  | (Compound(_), Compound(_, eq_class_lt, eq_class_rt)) =>
    target_typ_used_in_eq_class(target_typ, eq_class_lt)
    || target_typ_used_in_eq_class(target_typ, eq_class_rt)
    || target_typ_is_in_eq_typ(target_typ, eq_typ)
  | (Base(_) | Compound(_), Mapped(_, eq_class)) =>
    target_typ_used_in_eq_class(target_typ, eq_class)
  | (Base(_) | Mapped(_), Compound(_, eq_class_lt, eq_class_rt)) =>
    target_typ_is_in_eq_class(target_typ, eq_class_lt)
    || target_typ_is_in_eq_class(target_typ, eq_class_rt)
  };
}
and target_class_used_in_eq_class = (target_class: t, eq_class: t): bool => {
  // is target_class ⊆ eq_class?
  let target_typ_used = (target_typ: eq_typ): bool => {
    target_typ_used_in_eq_class(target_typ, eq_class);
  };
  // every target typ must be used in the eq class for the whole target class to have been used
  List.for_all(target_typ_used, target_class);
};

let rec target_typ_in_domain_but_not_equal =
        (eq_class: t, target_typ: eq_typ): bool => {
  List.exists(target_typ_in_domain_but_not_equal_typ(target_typ), eq_class);
}
and target_typ_in_domain_but_not_equal_typ =
    (target_typ: eq_typ, eq_typ: eq_typ): bool => {
  // is target_typ ⊂ eq_typ?
  // NOTE:
  //    target_typ != eq_typ ^ target_typ ⊆ eq_typ
  //    => target_typ ⊂ eq_typ
  !target_typ_is_in_eq_typ(target_typ, eq_typ)
  && target_typ_used_in_eq_typ(target_typ, eq_typ);
};

let is_known: eq_typ => bool =
  fun
  | Base(BUnknown(_)) => false
  | _ => true;

let rec filter_unneeded_holes_class =
        (comp: eq_typ => bool, remove: bool, eq_class: t): t => {
  switch (eq_class) {
  | [] => []
  | [hd, ...tl] =>
    let (had_hole, filtered_hd_opt) =
      filter_unneeded_holes_typ(comp, remove, hd);
    let remove = had_hole || remove;
    switch (filtered_hd_opt) {
    | None => filter_unneeded_holes_class(comp, remove, tl)
    | Some(filtered_hd) => [
        filtered_hd,
        ...filter_unneeded_holes_class(comp, remove, tl),
      ]
    };
  };
}
and filter_unneeded_holes_typ =
    (comp: eq_typ => bool, remove: bool, eq_typ: eq_typ)
    : (bool, option(eq_typ)) => {
  switch (eq_typ) {
  | Base(btyp) =>
    switch (btyp) {
    | BUnknown(_) =>
      let eq_tp_opt = remove ? None : Some(eq_typ);
      (true, eq_tp_opt);
    | _ => (false, Some(eq_typ))
    }
  | Mapped(ctor, eq_class) =>
    let delete_holes = List.exists(comp, eq_class);
    let eq_class = filter_unneeded_holes_class(comp, delete_holes, eq_class);
    (false, Some(Mapped(ctor, eq_class)));
  | Compound(ctor, eq_class_lt, eq_class_rt) =>
    let delete_holes_lt = List.exists(comp, eq_class_lt);
    let delete_holes_rt = List.exists(comp, eq_class_rt);
    let eq_class_lt =
      filter_unneeded_holes_class(comp, delete_holes_lt, eq_class_lt);
    let eq_class_rt =
      filter_unneeded_holes_class(comp, delete_holes_rt, eq_class_rt);
    (false, Some(Compound(ctor, eq_class_lt, eq_class_rt)));
  };
};

let filter_unneeded_holes = (comp: eq_typ => bool, eq_class: t): t => {
  let delete_holes = List.exists(comp, eq_class);
  filter_unneeded_holes_class(comp, delete_holes, eq_class);
};

let rec filtered_eq_class_to_typ: t => option(ITyp.t) =
  fun
  | [] => None
  | [Base(btyp)] => Some(btyp |> base_typ_to_ityp)
  | [Compound(ctor, eq_class_lt, eq_class_rt)] => {
      let* typ1 = filtered_eq_class_to_typ(eq_class_lt);
      let+ typ2 = filtered_eq_class_to_typ(eq_class_rt);
      mk_as_binary_ctor(ctor, typ1, typ2);
    }
  | [Mapped(ctor, eq_class)] => {
      let+ elt_typ = filtered_eq_class_to_typ(eq_class);
      mk_as_unary_ctor(ctor, elt_typ);
    }
  | _ => None;

let comp_eq_typ = (eq_typ1: eq_typ, eq_typ2: eq_typ): int => {
  let strip_id_from_prov: Typ.type_provenance => float =
    fun
    | SynSwitch(id)
    | TypeHole(id)
    | Internal(id) =>
      id == 0 ? (-2.0) : Float.sub(0.0, Float.div(1.0, float_of_int(id)))
    | _ => 0.0;

  let eq_typ_to_float: eq_typ => float =
    fun
    | Base(BInt)
    | Base(BUnit)
    | Base(BFloat)
    | Base(BString)
    | Base(BBool) => 1.0
    | Base(BUnknown(prov)) => strip_id_from_prov(prov)
    | Compound(_) => 2.0
    | Mapped(_) => 3.0;

  Stdlib.compare(eq_typ_to_float(eq_typ1), eq_typ_to_float(eq_typ2));
};

let rec sort_eq_class = (eq_class: t): t => {
  let eq_class = List.fast_sort(comp_eq_typ, eq_class);
  sort_eq_class_explore(eq_class);
}
and sort_eq_class_explore = (eq_class: t): t => {
  switch (eq_class) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Base(_) => [hd, ...sort_eq_class_explore(tl)]
    | Mapped(ctor, eq_class_arg) =>
      let sorted_class = sort_eq_class(eq_class_arg);
      [Mapped(ctor, sorted_class), ...sort_eq_class(tl)];
    | Compound(ctor, eq_class_lt, eq_class_rt) =>
      let sorted_class_lt = sort_eq_class(eq_class_lt);
      let sorted_class_rt = sort_eq_class(eq_class_rt);
      [
        Compound(ctor, sorted_class_lt, sorted_class_rt),
        ...sort_eq_class_explore(tl),
      ];
    }
  };
};

let string_of_btyp = (btyp: base_typ): string => {
  btyp |> base_typ_to_ityp |> ITyp.ityp_to_typ |> Typ.typ_to_string;
};

let rec string_of_eq_class = (eq_class: t): string =>
  switch (eq_class) {
  | [] => ""
  | [hd] => string_of_eq_typ(hd)
  | [hd, ...tl] =>
    let hd_str = string_of_eq_typ(hd);
    String.concat("//", [hd_str, string_of_eq_class(tl)]);
  }
and string_of_eq_typ = (eq_typ: eq_typ) =>
  switch (eq_typ) {
  | Base(btyp) => string_of_btyp(btyp)
  | Compound(ctor, eq_class_lt, eq_class_rt) =>
    let (ctor_start, ctor_string, ctor_end) =
      switch (ctor) {
      | CArrow => ("", " -> (", ")")
      | CProd => ("(", ", ", ")")
      | CSum => ("", " + (", ")")
      };

    String.concat(
      "",
      [
        ctor_start,
        string_of_eq_class(eq_class_lt),
        ctor_string,
        string_of_eq_class(eq_class_rt),
        ctor_end,
      ],
    );
  | Mapped(ctor, eq_class) =>
    let (start_text, end_text) =
      switch (ctor) {
      | CList => ("[", "]")
      };

    String.concat("", [start_text, string_of_eq_class(eq_class), end_text]);
  };
