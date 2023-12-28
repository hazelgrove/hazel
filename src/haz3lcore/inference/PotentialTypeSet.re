open Util;
open OptUtil.Syntax;
open Sexplib.Std;

/**
 * An PotentialTypeSet.t is a condensed representation of a list of types.
 * It can be a single type, or a composition of other PotentialTypeSet.t
 *
 * We use PotentialTypeSet to maintain all possible combinations of solutions during unification
 * and properly report errors/solutions without combinatorial explosion.
 * Inconsistent types and types failing an occurs check can be added to the same PotentialTypeSet without issue,
 * preventing unification from ever having to crash.
 */

[@deriving (show({with_path: false}), sexp)]
type base_typ =
  | BUnit
  | BInt
  | BFloat
  | BBool
  | BString
  | BVar(string)
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
type t = list(potential_typ)
and potential_typ =
  | Base(base_typ)
  | Unary(unary_ctor, t)
  | Binary(binary_ctor, t, t);

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

let rec ityp_to_potential_typ: ITyp.t => potential_typ =
  fun
  | Unknown(prov) => Base(BUnknown(prov))
  | Var(name) => Base(BVar(name))
  | Int => Base(BInt)
  | Unit => Base(BUnit)
  | Float => Base(BFloat)
  | Bool => Base(BBool)
  | String => Base(BString)
  | Arrow(ty1, ty2) =>
    Binary(
      CArrow,
      [ityp_to_potential_typ(ty1)],
      [ityp_to_potential_typ(ty2)],
    )
  | Prod(ty1, ty2) =>
    Binary(
      CProd,
      [ityp_to_potential_typ(ty1)],
      [ityp_to_potential_typ(ty2)],
    )
  | Sum(ty1, ty2) =>
    Binary(
      CProd,
      [ityp_to_potential_typ(ty1)],
      [ityp_to_potential_typ(ty2)],
    )
  | List(ty) => Unary(CList, [ityp_to_potential_typ(ty)]);

let typ_to_potential_typ: Typ.t => potential_typ =
  typ => {
    typ |> ITyp.typ_to_ityp |> ityp_to_potential_typ;
  };

let base_typ_to_ityp: base_typ => ITyp.t =
  fun
  | BInt => Int
  | BFloat => Float
  | BBool => Bool
  | BString => String
  | BUnit => Unit
  | BUnknown(prov) => Unknown(prov)
  | BVar(name) => Var(name);

let rec extend_with_potential_typ_set =
        (target: t, potential_typ_set_extension: t) => {
  switch (potential_typ_set_extension) {
  | [] => target
  | [potential_typ_extension, ...extension_tl] =>
    let target = extend_with_potential_typ(target, potential_typ_extension);
    extend_with_potential_typ_set(target, extension_tl);
  };
}
and extend_with_potential_typ =
    (target: t, potential_typ_extension: potential_typ) => {
  switch (target) {
  | [] => [potential_typ_extension]
  | [target_hd, ...target_tl] =>
    let extend_target_tl: unit => t = (
      () => {
        [
          target_hd,
          ...extend_with_potential_typ(target_tl, potential_typ_extension),
        ];
      }
    );
    switch (target_hd, potential_typ_extension) {
    | (_, Base(_)) =>
      target_hd == potential_typ_extension ? target : extend_target_tl()
    | (
        Unary(hd_ctor, hd_potential_typ_set),
        Unary(potential_typ_ctor, potential_typ_set),
      ) =>
      hd_ctor == potential_typ_ctor
        ? [
          Unary(
            hd_ctor,
            extend_with_potential_typ_set(
              hd_potential_typ_set,
              potential_typ_set,
            ),
          ),
          ...target_tl,
        ]
        : extend_target_tl()
    | (
        Binary(hd_ctor, hd_potential_typ_set_lt, hd_potential_typ_set_rt),
        Binary(
          potential_typ_ctor,
          potential_typ_set_lt,
          potential_typ_set_rt,
        ),
      ) =>
      if (hd_ctor == potential_typ_ctor) {
        let hd_potential_typ_set_lt =
          extend_with_potential_typ_set(
            hd_potential_typ_set_lt,
            potential_typ_set_lt,
          );
        let hd_potential_typ_set_rt =
          extend_with_potential_typ_set(
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

type split_result =
  | Success
  | Error(split_error_status)
and split_error_status =
  | Unsplittable
  | WrongCtor;

let split_potential_typ: potential_typ => option((t, t)) =
  fun
  | Unary(_)
  | Base(_) => None
  | Binary(_, potential_typ_set1, potential_typ_set2) =>
    Some((potential_typ_set1, potential_typ_set2));

// not currently in use but kept for utility
let split_potential_typ_set = (ctor_used: binary_ctor, potential_typ_set: t) => {
  let split_result_of: potential_typ => split_result =
    fun
    | Base(ty) =>
      switch (ty) {
      | BUnknown(_) => Success
      | _ => Error(Unsplittable)
      }
    | Unary(_) => Error(Unsplittable)
    | Binary(ctor, _, _) => ctor_used == ctor ? Success : Error(WrongCtor);

  let accumulate_splits =
      ((acc_class_lt, acc_class_rt): (t, t), potential_typ: potential_typ) => {
    switch (split_potential_typ(potential_typ)) {
    | None => (acc_class_lt, acc_class_rt)
    | Some((potential_typ_set_lt, potential_typ_set_rt)) =>
      let acc_class_lt =
        extend_with_potential_typ_set(acc_class_lt, potential_typ_set_lt);
      let acc_class_rt =
        extend_with_potential_typ_set(acc_class_rt, potential_typ_set_rt);
      (acc_class_lt, acc_class_rt);
    };
  };

  let (potential_typ_set_lt, potential_typ_set_rt) =
    List.fold_left(accumulate_splits, ([], []), potential_typ_set);

  // Unsplittable errors take precedence over WrongCtor due to strictly more severe error handling
  let rec check_ctor =
          (potential_typ_set: t, wrong_ctor_error_found: bool): split_result => {
    switch (potential_typ_set) {
    | [] => wrong_ctor_error_found ? Error(WrongCtor) : Success
    | [hd, ...tl] =>
      switch (split_result_of(hd)) {
      | Error(Unsplittable) as e => e
      | Error(WrongCtor) => check_ctor(tl, true)
      | _ => check_ctor(tl, wrong_ctor_error_found)
      }
    };
  };

  (
    check_ctor(potential_typ_set, false),
    potential_typ_set_lt,
    potential_typ_set_rt,
  );
};

let fuse =
    (ctor_used: binary_ctor, potential_typ_set_lt: t, potential_typ_set_rt: t) => {
  Binary(ctor_used, potential_typ_set_lt, potential_typ_set_rt);
};

let rec target_typ_is_in_potential_typ_set =
        (target_typ: potential_typ, potential_typ_set: t): bool => {
  // is target_typ ∈ potential_typ_set? this would make them equal (via transitivity)
  switch (potential_typ_set) {
  | [] => false
  | [hd, ...tl] =>
    target_typ_is_in_potential_typ(target_typ, hd)
    || target_typ_is_in_potential_typ_set(target_typ, tl)
  };
}
and target_typ_is_in_potential_typ =
    (target_typ: potential_typ, potential_typ: potential_typ): bool => {
  switch (target_typ, potential_typ) {
  | (_, Base(_)) => target_typ == potential_typ
  | (
      Unary(target_ctor, target_potential_typ_set),
      Unary(ctor, potential_typ_set),
    ) =>
    target_ctor == ctor
    && target_class_is_in_potential_typ_set(
         target_potential_typ_set,
         potential_typ_set,
       )
  | (
      Binary(target_ctor, target_class_lt, target_class_rt),
      Binary(ctor, potential_typ_set_lt, potential_typ_set_rt),
    ) =>
    target_ctor == ctor
    && target_class_is_in_potential_typ_set(
         target_class_lt,
         potential_typ_set_lt,
       )
    && target_class_is_in_potential_typ_set(
         target_class_rt,
         potential_typ_set_rt,
       )
  | (Base(_) | Binary(_), Unary(_))
  | (Base(_) | Unary(_), Binary(_)) => false
  };
}
and target_class_is_in_potential_typ_set =
    (target_class: t, potential_typ_set: t): bool => {
  // is target_class ∈ potential_typ_set? this would make them equal (via transitivity)
  let target_typ_contained = (target_typ: potential_typ): bool => {
    target_typ_is_in_potential_typ_set(target_typ, potential_typ_set);
  };
  List.for_all(target_typ_contained, target_class);
};

let rec target_typ_used_in_potential_typ_set =
        (target_typ: potential_typ, potential_typ_set: t): bool => {
  // is [target_typ] ⊆ potential_typ_set?
  switch (potential_typ_set) {
  | [] => false
  | [hd, ...tl] =>
    target_typ_used_in_potential_typ(target_typ, hd)
    || target_typ_used_in_potential_typ_set(target_typ, tl)
  };
}
and target_typ_used_in_potential_typ =
    (target_typ: potential_typ, potential_typ: potential_typ): bool => {
  // target used inside, or is represented by the potential_typ itself
  switch (target_typ, potential_typ) {
  | (_, Base(_)) => target_typ == potential_typ
  | (Unary(_), Unary(_, potential_typ_set)) =>
    target_typ_used_in_potential_typ_set(target_typ, potential_typ_set)
    || target_typ_is_in_potential_typ(target_typ, potential_typ)
  | (Binary(_), Binary(_, potential_typ_set_lt, potential_typ_set_rt)) =>
    target_typ_used_in_potential_typ_set(target_typ, potential_typ_set_lt)
    || target_typ_used_in_potential_typ_set(target_typ, potential_typ_set_rt)
    || target_typ_is_in_potential_typ(target_typ, potential_typ)
  | (Base(_) | Binary(_), Unary(_, potential_typ_set)) =>
    target_typ_used_in_potential_typ_set(target_typ, potential_typ_set)
  | (
      Base(_) | Unary(_),
      Binary(_, potential_typ_set_lt, potential_typ_set_rt),
    ) =>
    target_typ_is_in_potential_typ_set(target_typ, potential_typ_set_lt)
    || target_typ_is_in_potential_typ_set(target_typ, potential_typ_set_rt)
  };
}
and target_class_used_in_potential_typ_set =
    (target_class: t, potential_typ_set: t): bool => {
  // is target_class ⊆ potential_typ_set?
  let target_typ_used = (target_typ: potential_typ): bool => {
    target_typ_used_in_potential_typ_set(target_typ, potential_typ_set);
  };
  // every target typ must be used in the eq class for the whole target class to have been used
  List.for_all(target_typ_used, target_class);
};

let rec target_typ_in_domain_but_not_equal =
        (potential_typ_set: t, target_typ: potential_typ): bool => {
  List.exists(
    target_typ_in_domain_but_not_equal_typ(target_typ),
    potential_typ_set,
  );
}
and target_typ_in_domain_but_not_equal_typ =
    (target_typ: potential_typ, potential_typ: potential_typ): bool => {
  // is target_typ ⊂ potential_typ?
  // NOTE:
  //    target_typ != potential_typ ^ target_typ ⊆ potential_typ
  //    => target_typ ⊂ potential_typ
  !target_typ_is_in_potential_typ(target_typ, potential_typ)
  && target_typ_used_in_potential_typ(target_typ, potential_typ);
};

let is_known: potential_typ => bool =
  fun
  | Base(BUnknown(_)) => false
  | _ => true;

let rec filter_unneeded_holes_class =
        (comp: potential_typ => bool, remove: bool, potential_typ_set: t): t => {
  switch (potential_typ_set) {
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
    (comp: potential_typ => bool, remove: bool, potential_typ: potential_typ)
    : (bool, option(potential_typ)) => {
  switch (potential_typ) {
  | Base(btyp) =>
    switch (btyp) {
    | BUnknown(_) =>
      let eq_tp_opt = remove ? None : Some(potential_typ);
      (true, eq_tp_opt);
    | _ => (false, Some(potential_typ))
    }
  | Unary(ctor, potential_typ_set) =>
    let delete_holes = List.exists(comp, potential_typ_set);
    let potential_typ_set =
      filter_unneeded_holes_class(comp, delete_holes, potential_typ_set);
    (false, Some(Unary(ctor, potential_typ_set)));
  | Binary(ctor, potential_typ_set_lt, potential_typ_set_rt) =>
    let delete_holes_lt = List.exists(comp, potential_typ_set_lt);
    let delete_holes_rt = List.exists(comp, potential_typ_set_rt);
    let potential_typ_set_lt =
      filter_unneeded_holes_class(
        comp,
        delete_holes_lt,
        potential_typ_set_lt,
      );
    let potential_typ_set_rt =
      filter_unneeded_holes_class(
        comp,
        delete_holes_rt,
        potential_typ_set_rt,
      );
    (false, Some(Binary(ctor, potential_typ_set_lt, potential_typ_set_rt)));
  };
};

let filter_unneeded_holes =
    (comp: potential_typ => bool, potential_typ_set: t): t => {
  let delete_holes = List.exists(comp, potential_typ_set);
  filter_unneeded_holes_class(comp, delete_holes, potential_typ_set);
};

let filter_vars = (potential_typ_set: t): t => {
  let is_non_node =
    fun
    | Base(BVar(_))
    | Base(BUnknown(_)) => false
    | _ => true;

  let is_not_var =
    fun
    | Base(BVar(_)) => false
    | _ => true;

  let num_literals =
    potential_typ_set |> List.filter(is_non_node) |> List.length;

  switch (num_literals) {
  | n when n > 1 =>
    // do not filter vars; already unsolved, allow selection between similar aliases
    potential_typ_set
  | _ =>
    // must be solved. we arbitrarily filter out everything but the literal so it is assigned solved status
    List.filter(is_not_var, potential_typ_set)
  };
};

let rec filtered_potential_typ_set_to_typ: t => option(ITyp.t) =
  fun
  | [] => None
  | [Base(btyp)] => Some(btyp |> base_typ_to_ityp)
  | [Binary(ctor, potential_typ_set_lt, potential_typ_set_rt)] => {
      let* typ1 = filtered_potential_typ_set_to_typ(potential_typ_set_lt);
      let+ typ2 = filtered_potential_typ_set_to_typ(potential_typ_set_rt);
      mk_as_binary_ctor(ctor, typ1, typ2);
    }
  | [Unary(ctor, potential_typ_set)] => {
      let+ elt_typ = filtered_potential_typ_set_to_typ(potential_typ_set);
      mk_as_unary_ctor(ctor, elt_typ);
    }
  | _ => None;

let comp_potential_typ =
    (potential_typ1: potential_typ, potential_typ2: potential_typ): int => {
  let rec strip_id: Typ.type_provenance => string =
    fun
    | NoProvenance => ""
    | ExpHole(_, id)
    | TypeHole(id) => Id.to_string(id)
    | Matched(_, prov) => strip_id(prov);

  let potential_typ_to_string: potential_typ => string =
    fun
    | Base(BInt)
    | Base(BUnit)
    | Base(BFloat)
    | Base(BString)
    | Base(BBool) => "A"
    | Base(BUnknown(prov)) => strip_id(prov)
    | Base(BVar(name)) => name
    | Binary(_) => "B"
    | Unary(_) => "C";

  Stdlib.compare(
    potential_typ_to_string(potential_typ1),
    potential_typ_to_string(potential_typ2),
  );
};

let rec sort_potential_typ_set = (potential_typ_set: t): t => {
  let potential_typ_set =
    List.fast_sort(comp_potential_typ, potential_typ_set);
  sort_potential_typ_set_explore(potential_typ_set);
}
and sort_potential_typ_set_explore = (potential_typ_set: t): t => {
  switch (potential_typ_set) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Base(_) => [hd, ...sort_potential_typ_set_explore(tl)]
    | Unary(ctor, potential_typ_set_arg) =>
      let sorted_class = sort_potential_typ_set(potential_typ_set_arg);
      [Unary(ctor, sorted_class), ...sort_potential_typ_set(tl)];
    | Binary(ctor, potential_typ_set_lt, potential_typ_set_rt) =>
      let sorted_class_lt = sort_potential_typ_set(potential_typ_set_lt);
      let sorted_class_rt = sort_potential_typ_set(potential_typ_set_rt);
      [
        Binary(ctor, sorted_class_lt, sorted_class_rt),
        ...sort_potential_typ_set_explore(tl),
      ];
    }
  };
};

let string_of_btyp = (btyp: base_typ): string => {
  let typ_to_string = arg => Typ.typ_to_string(arg, false);
  btyp |> base_typ_to_ityp |> ITyp.ityp_to_typ |> typ_to_string;
};

let rec potential_typ_set_to_ityp_unroll = (id: Id.t, pts: t): list(ITyp.t) => {
  switch (pts) {
  | [] => [ITyp.Unknown(ExpHole(Internal, id))]
  | [hd] => [potential_typ_to_ityp(id, hd)]
  | _ => List.map(potential_typ_to_ityp(id), pts)
  };
}
and potential_typ_set_to_ityp_no_unroll = (id: Id.t, pts: t): ITyp.t => {
  switch (pts) {
  | [] => ITyp.Unknown(ExpHole(Internal, id))
  | [hd] => potential_typ_to_ityp(id, hd)
  | _ => ITyp.Unknown(ExpHole(Error, id))
  };
}
and potential_typ_to_ityp = (id: Id.t, ptyp: potential_typ): ITyp.t => {
  switch (ptyp) {
  | Base(btyp) => base_typ_to_ityp(btyp)
  | Unary(CList, t) => ITyp.List(potential_typ_set_to_ityp_no_unroll(id, t))
  | Binary(CArrow, t1, t2) =>
    ITyp.Arrow(
      potential_typ_set_to_ityp_no_unroll(id, t1),
      potential_typ_set_to_ityp_no_unroll(id, t2),
    )
  | Binary(CProd, t1, t2) =>
    ITyp.Prod(
      potential_typ_set_to_ityp_no_unroll(id, t1),
      potential_typ_set_to_ityp_no_unroll(id, t2),
    )
  | Binary(CSum, t1, t2) =>
    ITyp.Sum(
      potential_typ_set_to_ityp_no_unroll(id, t1),
      potential_typ_set_to_ityp_no_unroll(id, t2),
    )
  };
};

let rec string_of_potential_typ_set_no_nesting =
        (is_left_child, potential_typ_set: t): string =>
  switch (potential_typ_set) {
  | [] => ""
  | [hd] => string_of_potential_typ(is_left_child, hd)
  | [_hd, ..._tl] => "!"
  }
and string_of_potential_typ =
    (is_left_child: bool, potential_typ: potential_typ) =>
  switch (potential_typ) {
  | Base(btyp) => string_of_btyp(btyp)
  | Binary(ctor, potential_typ_set_lt, potential_typ_set_rt) =>
    let (ctor_start, ctor_string, ctor_end) =
      switch (ctor) {
      | CArrow => is_left_child ? ("(", " -> ", ")") : ("", " -> ", "")
      | CProd => ("(", ", ", ")")
      | CSum => is_left_child ? ("(", " + ", ")") : ("", " + ", "")
      };

    String.concat(
      "",
      [
        ctor_start,
        string_of_potential_typ_set_no_nesting(true, potential_typ_set_lt),
        ctor_string,
        string_of_potential_typ_set_no_nesting(false, potential_typ_set_rt),
        ctor_end,
      ],
    );
  | Unary(ctor, potential_typ_set) =>
    let (start_text, end_text) =
      switch (ctor) {
      | CList => ("[", "]")
      };

    String.concat(
      "",
      [
        start_text,
        string_of_potential_typ_set_no_nesting(false, potential_typ_set),
        end_text,
      ],
    );
  };

let strings_of_potential_typ_set = (potential_typ_set: t): list(string) =>
  List.map(string_of_potential_typ(false), potential_typ_set);
