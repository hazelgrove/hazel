open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Atom(Atom.cls)
  | Invalid
  | EmptyHole
  | CycleHole
  | MultiHole
  | SynSwitch
  | Internal
  | LArrow
  | RArrow
  | NProduct
  | MList
  | RForall
  | TupLabelProv
  | TupLabelArg
  | Join
  | Arrow
  | Prod
  | TupLabel
  | Label
  | Sum
  | List
  | Var
  | Constructor // Constructor does not exist on Typ.term it's being used here as a hack for the cursors inspector
  | Parens
  | Rec
  | Forall;

include TermBase.Typ;

let term_of: t => term = IdTagged.term_of;
let unwrap: t => (term, term => t) = IdTagged.unwrap;
let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let temp: term => t = IdTagged.temp;

let all_ids_temp = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) =>
      {
        term: exp.term,
        annotation: {
          ids: [Id.invalid],
        },
      }
      |> continue;
  map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f);
};

let (replace_temp, replace_temp_exp) = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) => IdTagged.replace_temp(exp) |> continue;
  (
    map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
    TermBase.Exp.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
  );
};

let hole = (tms: list(TermBase.Any.t)): TermBase.Typ.term =>
  switch (tms) {
  | [] => Unknown((Hole(EmptyHole): TermBase.Prov.term) |> IdTagged.fresh)
  | [_, ..._] =>
    Unknown((Hole(MultiHole(tms)): TermBase.Prov.term) |> IdTagged.fresh)
  };

let cls_of_term: Grammar.typ_term('a) => cls =
  fun
  | Unknown({term: Hole(Invalid(_)), _}) => Invalid
  | Unknown({term: Hole(EmptyHole), _}) => EmptyHole
  | Unknown({term: Hole(CycleHole), _}) => CycleHole
  | Unknown({term: Hole(MultiHole(_)), _}) => MultiHole
  | Unknown({term: SynSwitch, _}) => SynSwitch
  | Unknown({term: Internal, _}) => Internal
  | Unknown({term: LArrow(_), _}) => LArrow
  | Unknown({term: RArrow(_), _}) => RArrow
  | Unknown({term: NProduct(_), _}) => NProduct
  | Unknown({term: MList(_), _}) => MList
  | Unknown({term: RForall(_), _}) => RForall
  | Unknown({term: TupLabel(_), _}) => TupLabelProv
  | Unknown({term: TupLabelArg(_), _}) => TupLabelArg
  | Unknown({term: Join(_), _}) => Join
  | Atom(c) => Atom(c)
  | List(_) => List
  | Arrow(_) => Arrow
  | Var(_) => Var
  | Prod(_) => Prod
  | TupLabel(_) => TupLabel
  | Label(_) => Label
  | Parens(_) => Parens
  | Sum(_) => Sum
  | Rec(_) => Rec
  | Forall(_) => Forall;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type"
  | MultiHole => "Broken type"
  | EmptyHole => "Type hole"
  | CycleHole => "Cycle type hole"
  | SynSwitch => "Synthetic type"
  | Internal => "Internal type"
  | LArrow => "Left arrow prov type"
  | RArrow => "Right arrow prov type"
  | NProduct => "Tuple prov type"
  | MList => "List prov type"
  | RForall => "Right Forall prov type"
  | TupLabelProv => "Tuple label prov"
  | TupLabelArg => "Tuple arg prov"
  | Join => "Join prov"
  | Atom(_) => "Base type"
  | Var => "Type variable"
  | Constructor => "Sum constructor"
  | List => "List type"
  | Arrow => "Function type"
  | Prod => "Tuple type"
  | TupLabel => "Labeled tuple item type"
  | Label => "Label"
  | Sum => "Sum type"
  | Parens => "Parenthesized type"
  | Rec => "Recursive type"
  | Forall => "Forall type";

let rec is_arrow = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
  | TupLabel(_, typ) => is_arrow(typ)
  | Arrow(_) => true
  | Unknown(_)
  | Atom(_)
  | List(_)
  | Label(_)
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let is_atom = (ty: t): bool =>
  switch (ty.term) {
  | Atom(_) => true
  | Parens(_)
  | TupLabel(_)
  | Arrow(_)
  | Unknown(_)
  | List(_)
  | Label(_)
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };

let rec has_fun = (typ: t) =>
  switch (typ.term) {
  | Parens(typ) => has_fun(typ)
  | TupLabel(_, typ) => has_fun(typ)
  | Arrow(_)
  | Forall(_) => true
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | Var(_) => false
  | List(t) => has_fun(t)
  | Rec(_, t) => has_fun(t)
  | Sum(sm) =>
    List.exists(
      fun
      | ConstructorMap.Variant(_, _, Some(t)) => has_fun(t)
      | _ => false,
      sm,
    )
  | Prod(tys) => List.exists(has_fun, tys)
  };

let rec is_forall = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
  | TupLabel(_, typ) => is_forall(typ)
  | Forall(_) => true
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | Label(_)
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Rec(_) => false
  };
};

let is_void = (typ: t) =>
  switch (typ.term) {
  | Sum(ctrs) => ConstructorMap.is_empty(ctrs)
  | Rec(_, {term: Sum(ctrs), _}) => ConstructorMap.is_empty(ctrs)
  | _ => false
  };

/* Functions below this point assume that types have been through the to_typ function above */

[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: Id.t,
  ty: t,
};

/* Strip location information from a list of sources */
let of_source = List.map((source: source) => source.ty);

/* How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let join_type_provenance =
    (p1: Prov.t, p2: Prov.t)
    : (TermBase.type_provenance_t, list(equivalence)) =>
  if (p1 == p2) {
    (p1, []);
  } else {
    let join_prov = Join(p1, p2) |> Prov.fresh;
    let join_hole = Unknown(join_prov) |> temp;
    (
      join_prov,
      [
        Con(Unknown(p1) |> temp, join_hole),
        Con(Unknown(p2) |> temp, join_hole),
      ],
    );
  };

let rec match_tup_optional_label = (ty: t) =>
  switch (term_of(ty)) {
  | Parens(ty) => match_tup_optional_label(ty)
  | TupLabel({term: Label(name), _}, t') => Some((Some(name), t'))
  | TupLabel({term: Unknown(_), _}, t') => Some((None, t'))
  | Unknown(_) => Some((None, ty))
  | _ => None
  };
let match_tup_label = ty =>
  switch (match_tup_optional_label(ty)) {
  | Some((Some(name), t')) => Some((name, t'))
  | _ => None
  };

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Atom(_)
  | Label(_) => []
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | Parens(ty) => free_vars(~bound, ty)
  | List(ty) => free_vars(~bound, ty)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) => ConstructorMap.free_variables(free_vars(~bound), sm)
  | Prod(tys) => List.concat_map(free_vars(~bound), tys)
  | TupLabel(_, ty) => free_vars(~bound, ty)
  | Rec(x, ty)
  | Forall(x, ty) =>
    free_vars(~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound, ty)
  };

let rec vars = (ty: t): list(Var.t) =>
  switch (ty.term) {
  | Atom(_)
  | Unknown(_) => []
  | Var(x) => [x]
  | Arrow(ty1, ty2) => vars(ty1) @ vars(ty2)
  | Prod(tys) => List.concat_map(vars, tys)
  | Sum(sm) =>
    List.concat_map(
      fun
      | ConstructorMap.BadEntry(_) => []
      | Variant(_, _, None) => []
      | Variant(_, _, Some(typ)) => vars(typ),
      sm,
    )
  | Rec({term: Var(x), _}, ty) =>
    /* Remove recursive type references */
    vars(ty) |> List.filter((x': string) => x' != x)
  | Rec(_, ty) => vars(ty)
  | List(ty) => vars(ty)
  | Parens(ty) => vars(ty)
  | Forall({term: Var(x), _}, ty) =>
    vars(ty) |> List.filter((x': string) => x' != x)
  | Forall(_, ty) => vars(ty)
  | Label(_) => []
  | TupLabel(_, ty) => vars(ty)
  };

let rec aliases_deep = (ctx: Ctx.t, ty: t): list((string, t)) => {
  let defs =
    List.concat_map(
      var =>
        switch (Ctx.lookup_alias(ctx, var)) {
        | Some(ty) => [(var, ty)]
        | None => [
            (
              var,
              fresh(
                Unknown((Internal: TermBase.Prov.term) |> IdTagged.fresh),
              ),
            ),
          ]
        },
      vars(ty),
    )
    |> List.sort_uniq(((x, _), (y, _)) => compare(x, y));
  let rec_calls =
    List.concat_map(((_, ty')) => aliases_deep(ctx, ty'), defs);
  rec_calls @ defs;
};

let var_count = ref(0);
let fresh_var = (var_name: string) => {
  let x = var_count^;
  var_count := x + 1;
  var_name ++ "_α" ++ string_of_int(x);
};

/* Calculates the total number of nodes (compound
   and leaf) in the type AST. */
let rec num_nodes = (ty: t): int => {
  switch (ty.term) {
  | Atom(_)
  | Unknown(_) => 1
  | Var(_) => 1
  | Arrow(t1, t2) => 1 + num_nodes(t1) + num_nodes(t2)
  | Prod(tys) =>
    1 + List.fold_left((acc, ty) => acc + num_nodes(ty), 0, tys)
  | Sum(sm) =>
    1
    + List.fold_left(
        (acc, variant) =>
          switch (variant) {
          | ConstructorMap.BadEntry(_) => acc
          | Variant(_, _, ty) =>
            acc + Util.OptUtil.get(() => 0, Option.map(num_nodes, ty))
          },
        0,
        sm,
      )
  | Rec(_, ty) => 1 + num_nodes(ty)
  | List(ty) => 1 + num_nodes(ty)
  | Parens(ty) => 1 + num_nodes(ty)
  | Forall(_, ty) => 1 + num_nodes(ty)
  | Label(_) => 1
  | TupLabel(_, ty) => 1 + num_nodes(ty)
  };
};

/* Number of Unknown constructors in type AST */
let rec count_unknowns = (ty: t): int =>
  switch (ty.term) {
  | Unknown(_) => 1
  | Atom(_)
  | Var(_) => 0
  | Arrow(t1, t2) => count_unknowns(t1) + count_unknowns(t2)
  | Prod(tys) =>
    List.fold_left((acc, ty) => acc + count_unknowns(ty), 0, tys)
  | Sum(sm) =>
    List.fold_left(
      (acc, variant) =>
        switch (variant) {
        | ConstructorMap.BadEntry(_) => acc
        | Variant(_, _, ty) =>
          acc + Util.OptUtil.get(() => 0, Option.map(count_unknowns, ty))
        },
      0,
      sm,
    )
  | Rec(_, ty) => count_unknowns(ty)
  | List(ty) => count_unknowns(ty)
  | Parens(ty) => count_unknowns(ty)
  | Forall(_, ty) => count_unknowns(ty)
  | Label(_) => 0
  | TupLabel(_, ty) => count_unknowns(ty)
  };

let rec contains_sum_or_var = (ty: t): bool =>
  switch (ty.term) {
  | Atom(_)
  | Unknown(_) => false
  | Var(_)
  | Sum(_) => true
  | Arrow(t1, t2) => contains_sum_or_var(t1) || contains_sum_or_var(t2)
  | Prod(tys) => List.exists(contains_sum_or_var, tys)
  | Rec(_, ty) => contains_sum_or_var(ty)
  | List(ty) => contains_sum_or_var(ty)
  | Parens(ty) => contains_sum_or_var(ty)
  | Forall(_, ty) => contains_sum_or_var(ty)
  | Label(_) => false
  | TupLabel(_, ty) => contains_sum_or_var(ty)
  };

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) => subst(ty, tp, ty_body)
  | _ => ty
  };

/* Type Equality: This coincides with alpha equivalence for normalized types.
   Other types may be equivalent but this will not detect so if they are not normalized. */
let equal = (t1: t, t2: t): bool => fast_equal(t1, t2);

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join =
        (~resolve=false, ctx: Ctx.t, ty1: t, ty2: t)
        : option((t, list(equivalence))) => {
  let join' = join(~resolve, ctx);
  switch (term_of(ty1), term_of(ty2)) {
  | (_, Parens(ty2)) => join'(ty1, ty2)
  | (Parens(ty1), _) => join'(ty1, ty2)
  | (Unknown(p1), Unknown(p2)) =>
    let (prov, cons) = join_type_provenance(p1, p2);
    Some((Unknown(prov) |> temp, cons));
  | (Unknown(_), _) => Some((ty2, []))
  | (_, Unknown(_)) => Some((ty1, []))
  | (Var(n1), Var(n2)) =>
    if (n1 == n2) {
      Some((ty1, []));
    } else {
      let* ty1 = Ctx.lookup_alias(ctx, n1);
      let* ty2 = Ctx.lookup_alias(ctx, n2);
      let+ (ty_join, cons) = join'(ty1, ty2);
      (!resolve && equal(ty1, ty_join) ? ty1 : ty_join, cons);
    }
  | (Var(name), _) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ (ty_join, cons) = join'(ty_name, ty2);
    (!resolve && equal(ty_name, ty_join) ? ty1 : ty_join, cons);
  | (_, Var(name)) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ (ty_join, cons) = join'(ty_name, ty1);
    (!resolve && equal(ty_name, ty_join) ? ty2 : ty_join, cons);
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
      | None => ty1
      };
    let+ (ty_body, cons) = join(~resolve, ctx, ty1', ty2);
    (Rec(tp1, ty_body) |> temp, cons);
  | (Rec(_), _) => None
  | (Forall(x1, ty1), Forall(x2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let ctx = Ctx.extend_dummy_tvar(ctx, x2);
    let+ (ty_body, join_cons) = join(~resolve, ctx, ty1', ty2);
    (Forall(x2, ty_body) |> temp, join_cons);
  /* Note for above: there is no danger of free variable capture as
     subst itself performs capture avoiding substitution. However this
     may generate internal type variable names that in corner cases can
     be exposed to the user. We preserve the variable name of the
     second type to preserve synthesized type variable names, which
     come from user annotations. */
  | (Forall(_), _) => None
  | (Atom(c1), Atom(c2)) when c1 == c2 => Some((ty1, []))
  | (Atom(_), _) => None
  | (Label(_), Label("")) => Some((ty1, []))
  | (Label(""), Label(_)) => Some((ty2, []))
  | (Label(name1), Label(name2))
      when LabeledTuple.match_labels(name1, name2) =>
    Some((ty1, []))
  | (Label(_), _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* (ty1, join_cons1) = join'(ty1, ty1');
    let+ (ty2, join_cons2) = join'(ty2, ty2');
    (Arrow(ty1, ty2) |> temp, join_cons1 @ join_cons2);
  | (Arrow(_), _) => None
  | (TupLabel(lab1, ty1'), TupLabel(lab2, ty2')) =>
    let* (lab, lab_cons) = join'(lab1, lab2);
    let+ (ty, ty_cons) = join'(ty1', ty2');
    (TupLabel(lab, ty) |> temp, lab_cons @ ty_cons);
  | (TupLabel(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      let* tys = ListUtil.map2_opt(join', tys1, tys2);
      let* tys = OptUtil.sequence(tys);
      let+ (tys, ty_cons) = Some(List.split(tys));
      (Prod(tys) |> temp, List.flatten(ty_cons));
    }
  | (Prod(_), _) => None
  | (Sum(sm1), Sum(sm2)) =>
    let join = (a, b) => {
      let+ (joined, _) = join(~resolve, ctx, a, b);
      joined;
    };
    let+ sm' = ConstructorMap.join(equal, join, sm1, sm2);
    (Sum(sm') |> temp, []);
  | (Sum(_), _) => None
  | (List(ty1), List(ty2)) =>
    let+ (ty, ty_cons) = join'(ty1, ty2);
    (List(ty) |> temp, ty_cons);
  | (List(_), _) => None
  };
};

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by matching against t2 */
let rec match_synswitch = (t1: t, t2: t) => {
  let (term1, rewrap1) = unwrap(t1);
  switch (term1, term_of(t2)) {
  | (Parens(t1), _) => Parens(match_synswitch(t1, t2)) |> rewrap1
  | (Unknown({term: SynSwitch, _}), _) => t2
  // These cases can't have a synswitch inside
  | (Unknown(_), _)
  | (Atom(_), _)
  | (Label(_), _)
  | (Var(_), _)
  | (Rec(_), _) => t1
  // These might
  | (List(ty1), List(ty2)) => List(match_synswitch(ty1, ty2)) |> rewrap1
  | (List(_), _) => t1
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    Arrow(match_synswitch(ty1, ty1'), match_synswitch(ty2, ty2')) |> rewrap1
  | (Arrow(_), _) => t1
  | (Prod(tys1), Prod(tys2)) when List.length(tys1) == List.length(tys2) =>
    let tys = List.map2(match_synswitch, tys1, tys2);
    Prod(tys) |> rewrap1;
  | (Prod(_), _) => t1
  | (TupLabel(label1, ty1), TupLabel(label2, ty2)) =>
    TupLabel(match_synswitch(label1, label2), match_synswitch(ty1, ty2))
    |> rewrap1
  | (TupLabel(_, _), _) => t1
  | (Sum(sm1), Sum(sm2)) =>
    let sm' =
      ConstructorMap.match_synswitch(match_synswitch, equal, sm1, sm2);
    Sum(sm') |> rewrap1;
  | (Sum(_), _) => t1
  // HACK[Matt]: The only possible forall is `Forall Syn -> Syn`
  | (Forall(_), Forall(_)) => t2
  | (Forall(_), _) => t1
  };
};

let join_all =
    (~empty: t, ctx: Ctx.t, ts: list(t)): option((t, list(equivalence))) => {
  let+ (ty, cons) =
    List.fold_left(
      (acc: option((t, list(list(equivalence)))), ty: t) => {
        let* (acc_ty, acc_cons) = acc;
        let+ (join_ty, join_cons) = join(ctx, ty, acc_ty);
        (join_ty, [join_cons, ...acc_cons]);
      },
      Some((empty, [])),
      ts,
    );

  (ty, List.flatten(cons));
};

let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
  join(ctx, ty1, ty2) != None;

/**
   * Determines if one type (`ty1`) is more precise than another type (`ty2`) within a given context (`ctx`).
   *
   * @return - `true` if `ty1` is more precise than `ty2`, otherwise `false`.
   */
let is_more_precise = (ctx: Ctx.t, ty1: t, ty2: t): bool => {
  let joined = join(ctx, ty1, ty2);
  switch (joined) {
  | None => false
  | Some((joined, _)) => fast_equal(~alpha_equivalence=true, joined, ty1)
  };
};

let rec weak_head_normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t => {
  if (rec_counter > 1000) {
    failwith("weak_head_normalize exceeded 1000 recursive calls");
  };
  switch (term_of(ty)) {
  | Parens(t) => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t)
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty)
    | None => ty
    }
  | _ => ty
  };
};

let rec normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t => {
  if (rec_counter > 1000) {
    failwith("normalize exceeded 1000 recursive calls");
  };
  let normalize = normalize(~rec_counter=rec_counter + 1);
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Unknown(_)
  | Atom(_)
  | Label(_) => ty
  | Parens(t) => normalize(ctx, t)
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(normalize(ctx), ts)) |> rewrap
  | TupLabel(label, ty) =>
    TupLabel(normalize(ctx, label), normalize(ctx, ty)) |> rewrap
  | Sum(ts) =>
    Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts)) |> rewrap
  | Rec(tpat, ty) =>
    /* NOTE: Dummy tvar added has fake id but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(tpat, normalize(Ctx.extend_dummy_tvar(ctx, tpat), ty)) |> rewrap
  | Forall(name, ty) =>
    Forall(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
  };
};

let matched_arrow_of_prov = ({term: t, annotation}: Prov.t, ty: t) => {
  let left_arr =
    Unknown({
      term: LArrow(t),
      annotation,
    })
    |> temp;
  let right_arr =
    Unknown({
      term: RArrow(t),
      annotation,
    })
    |> temp;
  (left_arr, right_arr, [Con(ty, Arrow(left_arr, right_arr) |> temp)]);
};

let rec matched_arrow_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_arrow_strict(ctx, ty)
  | Arrow(ty_in, ty_out) => Some((ty_in, ty_out, []))
  | Unknown({term: SynSwitch, _} as prov) =>
    Some(matched_arrow_of_prov(prov, ty))
  | _ => None
  };

let matched_arrow = (ctx, ty) => {
  switch (matched_arrow_strict(ctx, ty)) {
  | Some(v) => v
  | None =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Unknown({term: t, annotation}) =>
      matched_arrow_of_prov(
        {
          term: t,
          annotation,
        },
        ty,
      )
    | _ =>
      let prov = (Internal: TermBase.Prov.term) |> IdTagged.temp;
      matched_arrow_of_prov(prov, ty);
    }
  };
};

let matched_forall_of_prov = (prov, ty) => {
  (
    None,
    ty,
    [
      Con(
        ty,
        Forall(EmptyHole |> TPat.fresh, Unknown(prov) |> temp) |> temp,
      ),
    ],
  );
};

let rec matched_forall_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_forall_strict(ctx, ty)
  | Forall(t, ty) => Some((Some(t), ty, []))
  | Unknown({term: SynSwitch, annotation}) =>
    Some(
      matched_forall_of_prov(
        {
          term: RForall(SynSwitch),
          annotation,
        },
        ty,
      ),
    )
  | _ => None
  };

// TODO: (THI) does this need constraints and special provenances?
let matched_forall = (ctx, ty) =>
  // TODO: want to optimize repeated term_of/normalize calls
  switch (matched_forall_strict(ctx, ty)) {
  | Some(r) => r
  | None =>
    let prov =
      switch (term_of(weak_head_normalize(ctx, ty))) {
      | Unknown(prov) => prov
      | _ => Internal |> Prov.fresh
      };
    matched_forall_of_prov(prov, ty);
  };

let rec get_labels = (ctx, ty): list(option(string)) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
  | Parens(ty) => get_labels(ctx, ty)
  | Prod(tys) => List.map(x => Option.map(fst, match_tup_label(x)), tys)
  | _ => []
  };
};

// TODO: (THI) document
let matched_prod_of_prov =
    ({term, annotation}: TermBase.type_provenance_t, es, ty) => {
  let prod_provs =
    List.init(List.length(es), n =>
      Unknown({
        term: NProduct(n, term),
        annotation,
      })
      |> temp
    );
  (prod_provs, [Con(ty, Prod(prod_provs) |> temp)]);
};

let rec matched_prod_strict:
  type a.
    (Ctx.t, list(a), a => option((string, a)), t, (string, a) => a) =>
    (list(a), option(list(t)), list(equivalence)) =
  (ctx: Ctx.t, es, get_label_es, ty: t, constructor) => {
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) =>
      matched_prod_strict(ctx, es, get_label_es, ty, constructor)
    | Prod(tys: list(t)) =>
      if (List.length(es) != List.length(tys)) {
        (es, None, []);
      } else {
        (
          LabeledTuple.rearrange(
            match_tup_label,
            get_label_es,
            tys,
            es,
            constructor,
          ),
          Some(tys),
          [],
        );
      }
    | Unknown({term: SynSwitch, _} as p) =>
      let (provs, constraints) = matched_prod_of_prov(p, es, ty);
      (es, Some(provs), constraints);
    | _ => (es, None, [])
    };
  };

let matched_prod = (ctx, es, get_label_es, ty, constructor) => {
  let (es, tys_opt, constraints) =
    matched_prod_strict(ctx, es, get_label_es, ty, constructor);
  let (a, constraints') =
    switch (tys_opt) {
    | Some(p) => (p, constraints)
    | None =>
      switch (term_of(weak_head_normalize(ctx, ty))) {
      | Unknown(p) => matched_prod_of_prov(p, es, ty)
      | _ =>
        let prov = Internal |> Prov.fresh;
        matched_prod_of_prov(prov, es, ty);
      }
    };
  (es, a, constraints');
};

// TODO: (THI) document
let matched_list_hole_of_prov =
    ({term, annotation}: TermBase.type_provenance_t, ty) => {
  let list_ty =
    Unknown({
      term: MList(term),
      annotation,
    })
    |> temp;
  (list_ty, [Con(ty, List(list_ty) |> temp)]);
};

let rec matched_list_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_list_strict(ctx, ty)
  | List(ty) => (Some(ty), [])
  | Unknown(prov) when prov.term == SynSwitch =>
    let (list_ty, constraints) = matched_list_hole_of_prov(prov, ty);
    (Some(list_ty), constraints);
  | _ => (None, [])
  };

let matched_list_strict_without_constraints = (ctx, ty) =>
  switch (matched_list_strict(ctx, ty)) {
  | (Some(ty), _) => Some(ty)
  | (None, _) => None
  };

let matched_list = (ctx, ty) => {
  let (list_ty_opt, constraints) = matched_list_strict(ctx, ty);
  let (list_ty, constraints') =
    switch (list_ty_opt) {
    | Some(list_ty) => (list_ty, constraints)
    | None =>
      switch (term_of(weak_head_normalize(ctx, ty))) {
      | Unknown(prov) => matched_list_hole_of_prov(prov, ty)
      | _ =>
        let prov = Internal |> Prov.fresh;
        matched_list_hole_of_prov(prov, ty);
      }
    };

  (list_ty, constraints');
  // |> Option.value(~default=Unknown(Internal |> Prov.fresh) |> temp);
};

// TODO: (THI) does this need constraints and special provenances?
let rec matched_args_strict = (ctx, ty, arity): Either.t('a, int) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_args_strict(ctx, ty, arity)
  | Prod(tys) when List.length(tys) == arity => L(tys)
  | Prod(tys) => R(List.length(tys))
  | _ when arity == 1 => L([ty])
  | Unknown(_) =>
    L(List.init(arity, _ => Unknown(Internal |> Prov.fresh) |> temp))
  | _ => R(1)
  };
};

let matched_label = (ctx, ty): option((t, t, list(equivalence))) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | TupLabel({term: Label(ml), _}, ty) => Some((Label(ml) |> temp, ty, []))
  | Unknown({term: t, annotation}) when t == SynSwitch =>
    let label =
      Unknown({
        term: TupLabel(t),
        annotation,
      })
      |> temp;
    let arg =
      Unknown({
        term: TupLabelArg(t),
        annotation,
      })
      |> temp;
    Some((label, arg, [Con(ty, TupLabel(label, arg) |> temp)]));
  | _ => None
  };

let rec get_sum_constructors = (ctx: Ctx.t, ty: t): option(sum_map) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
  | Parens(ty) => get_sum_constructors(ctx, ty)
  | Sum(sm) => Some(sm)
  | Rec(_) =>
    /* Note: We must unroll here to get right ctr types;
       otherwise the rec parameter will leak. However, seeing
       as substitution is too expensive to be used here, we
       currently making the optimization that, since all
       recursive types are type alises which use the alias name
       as the recursive parameter, and type aliases cannot be
       shadowed, it is safe to simply remove the Rec constructor,
       provided we haven't escaped the context in which the alias
       is bound. If either of the above assumptions become invalid,
       the below code will be incorrect! */
    let ty =
      switch (ty |> term_of) {
      | Rec({term: Var(x), _}, _ty_body) =>
        switch (Ctx.lookup_alias(ctx, x)) {
        | None => unroll(ty)
        | Some(_) => unroll(ty)
        }
      | _ => ty
      };
    switch (ty |> term_of) {
    | Sum(sm) => Some(sm)
    | _ => None
    };
  | _ => None
  };
};

let rec is_syn = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_syn(x)
  | Unknown(p) => is_prov_syn(p |> Prov.term_of)
  | Atom(_)
  | Label(_)
  | Var(_)
  | Rec(_)
  | Forall(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | Sum(_) => false
  }
and is_prov_syn = (prov: Prov.term): bool => {
  switch (prov) {
  | LArrow(p)
  | RArrow(p)
  | NProduct(_, p)
  | RForall(p)
  | TupLabel(p)
  | TupLabelArg(p)
  | MList(p) => is_prov_syn(p)
  | Join(p1, p2) =>
    is_prov_syn(p1 |> Prov.term_of) || is_prov_syn(p2 |> Prov.term_of)
  | SynSwitch => true
  | Internal => false
  | Hole(_) => false
  };
};

let rec is_ana_atom = (ty: t) =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_ana_atom(x)
  | Atom(a) => Some(a)
  | Unknown(_)
  | Label(_)
  | Var(_)
  | Rec(_)
  | Forall(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | Sum(_) => None
  };

let rec is_syn_plus = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_syn_plus(x)
  | Unknown(p) => is_prov_syn(p |> Prov.term_of)
  | Arrow(t1, t2) => is_syn(t1) && is_syn_plus(t2)
  | Forall(_, t) => is_syn(t)
  | Atom(_)
  | Label(_)
  | Var(_)
  | Rec(_)
  | List(_)
  | Prod(_)
  | Sum(_) => false
  };

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(ty) => needs_parens(ty)
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | TupLabel(_, _)
  | List(_) /* is already wrapped in [] */
  | Var(_) => false
  | Rec(_, _)
  | Forall(_, _)
  | Arrow(_, _)
  | Prod(_)
  | Sum(_) => true /* disambiguate between (A + B) -> C and A + (B -> C) */
  };

let pretty_print_tvar = (tv: TPat.t): string =>
  switch (IdTagged.term_of(tv)) {
  | Var(x) => x
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => "?"
  };

/* Essentially recreates web/view/Type.re's view_ty but with string output */
let rec pretty_print = (ty: t): string =>
  switch (term_of(ty)) {
  | Parens(ty) => pretty_print(ty)
  | Unknown(_) => "?"
  | Atom(Int) => "Int"
  | Atom(Float) => "Float"
  | Atom(Bool) => "Bool"
  | Atom(String) => "String"
  | Atom(Nat) => "Nat"
  | Atom(SInt) => "SInt"
  | Var(tvar) => tvar
  | List(t) => "[" ++ pretty_print(t) ++ "]"
  | Arrow(t1, t2) => paren_pretty_print(t1) ++ " -> " ++ pretty_print(t2)
  | Sum(sm) =>
    switch (sm) {
    | [] => "+?"
    | [t0] => "+" ++ ctr_pretty_print(t0)
    | [t0, ...ts] =>
      List.fold_left(
        (acc, t) => acc ++ " + " ++ ctr_pretty_print(t),
        ctr_pretty_print(t0),
        ts,
      )
    }
  | Prod([]) => "()"
  | Prod([t0, ...ts]) =>
    "("
    ++ List.fold_left(
         (acc, t) => acc ++ ", " ++ pretty_print(t),
         pretty_print(t0),
         ts,
       )
    ++ ")"
  | Label(name) => name
  | TupLabel(label, t) => pretty_print(label) ++ "=" ++ pretty_print(t)
  | Rec(tv, t) =>
    "rec " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | Forall(tv, t) =>
    "forall " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  }
and ctr_pretty_print =
  fun
  | ConstructorMap.Variant(ctr, _, None) => ctr
  | ConstructorMap.Variant(ctr, _, Some(t)) =>
    ctr ++ "(" ++ pretty_print(t) ++ ")"
  | ConstructorMap.BadEntry(_) => "?"
and paren_pretty_print = typ =>
  if (needs_parens(typ)) {
    "(" ++ pretty_print(typ) ++ ")";
  } else {
    pretty_print(typ);
  };

/**
 * Removes duplicate labels from a given list of types inside a tuple.
 *
 * This function takes a list of types and returns a new list with all
 * duplicate labels replaced with their first occurence and the unknown type.
 *
 * @param duplicate_labels - The list of duplicate labels.
 * @param tys - The list of types to remove duplicates from.
 * @return A new list of types with duplicates removed.
 */
let remove_duplicate_labels =
    (~duplicate_labels: list(LabeledTuple.label), tys: list(t)): list(t) => {
  snd(
    List.fold_left(
      ((seen_duplicates, deduplicated_types), ty) => {
        let tup_label = match_tup_label(ty);
        switch (tup_label) {
        | Some((l, _))
            when
              List.mem(l, duplicate_labels) && List.mem(l, seen_duplicates) => (
            seen_duplicates,
            deduplicated_types,
          )
        | Some((l, _)) when List.mem(l, duplicate_labels) => (
            [l] @ seen_duplicates,
            deduplicated_types
            @ [
              TupLabel(
                Label(l) |> temp,
                Unknown(
                  (Internal: TermBase.type_provenance) |> IdTagged.fresh,
                )
                |> temp,
              )
              |> temp,
            ],
          )
        | Some(_) => (seen_duplicates, deduplicated_types @ [ty])
        | None => (seen_duplicates, deduplicated_types @ [ty])
        };
      },
      ([], []),
      tys,
    ),
  );
};

/**
 * Converts a list of types (`tys`) into a product type.
 *
 * If the list contains a single type, it is returned as-is since singleton
 * products are not supported.
 *
 * @param tys - A list of types to be combined into a product type.
 * @return A product type representing the combination of the input types,
 *         or the single type if the list contains only one element.
 */
let to_product = (tys: list(t)): t =>
  switch (tys) {
  | []
  | [{term: TupLabel(_), _}] => Prod(tys) |> temp
  | [ty] => ty
  | _ => Prod(tys) |> temp
  };
