open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Atom(Atom.cls)
  | Invalid
  | EmptyHole
  | MultiHole
  | SynSwitch
  | Internal
  | Arrow
  | Prod
  | TupLabel
  | Label
  | ExplicitNonlabel
  | Sum
  | List
  | Var
  | Constructor // Constructor does not exist on Typ.term it's being used here as a hack for the cursors inspector
  | Parens
  | Projector
  | Rec
  | Poly
  | ProofOf
  | ProdProjection
  | ProdExtension;

include TermBase.Typ;

let term_of: t => term = IdTagged.term_of;
let unwrap: t => (term, term => t) = IdTagged.unwrap;
let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp,
  };

let all_ids_temp = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) =>
      {
        term: exp.term,
        annotation: IdTagged.IdTag.temp,
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
  | [] => Unknown(Hole(EmptyHole))
  | [_, ..._] => Unknown(Hole(MultiHole(tms)))
  };

let cls_of_term: Grammar.typ_term('a) => cls =
  fun
  | Unknown(Hole(Invalid(_))) => Invalid
  | Unknown(Hole(EmptyHole)) => EmptyHole
  | Unknown(Hole(MultiHole(_))) => MultiHole
  | Unknown(SynSwitch) => SynSwitch
  | Unknown(Internal) => Internal
  | Atom(c) => Atom(c)
  | List(_) => List
  | Arrow(_) => Arrow
  | Var(_) => Var
  | Prod(_) => Prod
  | TupLabel(_) => TupLabel
  | Label(_) => Label
  | ExplicitNonlabel => ExplicitNonlabel
  | Parens(_) => Parens
  | Projector(_) => Projector
  | Sum(_) => Sum
  | Rec(_) => Rec
  | Poly(_) => Poly
  | ProofOf(_) => ProofOf
  | ProdProjection(_) => ProdProjection
  | ProdExtension(_) => ProdExtension;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type"
  | MultiHole => "Broken type"
  | EmptyHole => "Type hole"
  | SynSwitch => "Synthetic type"
  | Internal => "Internal type"
  | Atom(_) => "Base type"
  | Var => "Type variable"
  | Constructor => "Sum constructor"
  | List => "List type"
  | Arrow => "Function type"
  | Prod => "Tuple type"
  | TupLabel => "Tuple item type"
  | Label => "Label"
  | ExplicitNonlabel => "Explicitly unlabeled tuple item type"
  | Sum => "Sum type"
  | Parens => "Parenthesized type"
  | Projector => "Projector type"
  | Rec => "Recursive type"
  | Poly => "Type quantifier"
  | ProofOf => "Proof type"
  | ProdProjection => "Tuple projection"
  | ProdExtension => "Tuple extension";

let rec is_arrow = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ) => is_arrow(typ)
  | Arrow(_) => true
  | Unknown(_)
  | Atom(_)
  | List(_)
  | Label(_)
  | ExplicitNonlabel
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Poly(_)
  | ProofOf(_)
  | Rec(_)
  | ProdProjection(_)
  | ProdExtension(_) => false
  };
};

let is_atom = (ty: t): bool =>
  switch (ty.term) {
  | Atom(_) => true
  | ProofOf(_)
  | Parens(_)
  | Projector(_)
  | TupLabel(_)
  | Arrow(_)
  | Unknown(_)
  | List(_)
  | Label(_)
  | ExplicitNonlabel
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Poly(_)
  | Rec(_)
  | ProdProjection(_)
  | ProdExtension(_) => false
  };

let rec has_fun = (typ: t) =>
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ)
  | ProdProjection(typ, _) => has_fun(typ)
  | Arrow(_)
  | Poly(_)
  | ProofOf(_) => true
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | ExplicitNonlabel
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
  | ProdExtension(t1, t2) => has_fun(t1) || has_fun(t2)
  };

let rec is_poly = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ) => is_poly(typ)
  | Poly(_) => true
  | ProofOf(_)
  | Unknown(_)
  | Atom(_)
  | Arrow(_)
  | List(_)
  | Label(_)
  | ExplicitNonlabel
  | Prod(_)
  | Var(_)
  | Sum(_)
  | Rec(_)
  | ProdProjection(_)
  | ProdExtension(_) => false
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
   meeting unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let meet_type_provenance =
    (p1: TermBase.type_provenance, p2: TermBase.type_provenance)
    : TermBase.type_provenance =>
  switch (p1, p2) {
  | (Hole(h1), Hole(h2)) when h1 == h2 => Hole(h1)
  | (Hole(EmptyHole), Hole(EmptyHole) | SynSwitch)
  | (SynSwitch, Hole(EmptyHole)) => Hole(EmptyHole)
  | (SynSwitch, Internal)
  | (Internal, SynSwitch) => SynSwitch
  | (Internal | Hole(_), _)
  | (_, Hole(_)) => Internal
  | (SynSwitch, SynSwitch) => SynSwitch
  };

let rec match_tup_optional_label = (ty: t) =>
  switch (term_of(ty)) {
  | Parens(ty) => match_tup_optional_label(ty)
  | TupLabel({term: Label(name), _}, t') => Some((Some(name), t'))
  | TupLabel({term: Unknown(_), _}, t') => Some((None, t'))
  | Unknown(_) => Some((None, ty))
  | _ => None
  };
let match_tup_label = ty => {
  switch (match_tup_optional_label(ty)) {
  | Some((Some(name), t')) => Some((name, t'))
  | _ => None
  };
};

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | ExplicitNonlabel => []
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | Parens(ty)
  | Projector(_, ty) => free_vars(~bound, ty)
  | List(ty) => free_vars(~bound, ty)
  | ProdExtension(t1, t2)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) => ConstructorMap.free_variables(free_vars(~bound), sm)
  | Prod(tys) => List.concat_map(free_vars(~bound), tys)
  | ProdProjection(t1, _) => free_vars(~bound, t1)
  | TupLabel(_, ty) => free_vars(~bound, ty)
  | Rec(x, ty)
  | Poly(x, ty) =>
    free_vars(~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound, ty)
  | ProofOf(_) => []
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
  | Parens(ty)
  | Projector(_, ty) => vars(ty)
  | Poly({term: Var(x), _}, ty) =>
    vars(ty) |> List.filter((x': string) => x' != x)
  | Poly(_, ty) => vars(ty)
  | ProofOf(_) => []
  | ExplicitNonlabel
  | Label(_) => []
  | TupLabel(_, ty)
  | ProdProjection(ty, _) => vars(ty)
  | ProdExtension(ty1, ty2) => vars(ty1) @ vars(ty2)
  };
let rec aliases_deep = (ctx: Ctx.t, ty: t): list((string, t)) => {
  let defs =
    List.concat_map(
      var =>
        switch (Ctx.lookup_alias(ctx, var)) {
        | Some(ty) => [(var, ty)]
        | None => [(var, fresh(Unknown(Internal)))]
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
  | Parens(ty)
  | Projector(_, ty) => 1 + num_nodes(ty)
  | Poly(_, ty) => 1 + num_nodes(ty)
  | ExplicitNonlabel
  | Label(_) => 1
  | TupLabel(_, ty) => 1 + num_nodes(ty)
  | ProofOf(_) => 10 // TODO[Matt]: this is a hack to make sure that Yes types are not counted as small
  | ProdProjection(ty1, ty2) => 1 + num_nodes(ty1) + num_nodes(ty2)
  | ProdExtension(ty1, ty2) => 1 + num_nodes(ty1) + num_nodes(ty2)
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
  | Parens(ty)
  | Projector(_, ty) => count_unknowns(ty)
  | Poly(_, ty) => count_unknowns(ty)
  | ProofOf(_) => 0
  | ExplicitNonlabel
  | Label(_) => 0
  | TupLabel(_, ty) => count_unknowns(ty)
  | ProdProjection(ty1, _) => count_unknowns(ty1)
  | ProdExtension(ty1, ty2) => count_unknowns(ty1) + count_unknowns(ty2)
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
  | Parens(ty)
  | Projector(_, ty) => contains_sum_or_var(ty)
  | Poly(_, ty) => contains_sum_or_var(ty)
  | ProofOf(_) => false
  | ProdProjection(ty1, _) => contains_sum_or_var(ty1)
  | ProdExtension(ty1, ty2) =>
    contains_sum_or_var(ty1) || contains_sum_or_var(ty2)
  | ExplicitNonlabel
  | Label(_) => false
  | TupLabel(_, ty) => contains_sum_or_var(ty)
  };

let rec subst = (s: t, x: TPat.t, ty: t): t => {
  switch (TPat.tyvar_of_utpat(x)) {
  | Some(str) =>
    let (term, rewrap) = Grammar.Annotated.unwrap(ty);
    switch (term) {
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel
    | Unknown(_)
    | ProofOf(_) => ty
    | Arrow(ty1, ty2) =>
      let ty1' = subst(s, x, ty1);
      let ty2' = subst(s, x, ty2);
      if (ty1' === ty1 && ty2' === ty2) {
        ty;
      } else {
        Grammar.Arrow(ty1', ty2') |> rewrap;
      };
    | Prod(tys) =>
      let tys' = List.map(subst(s, x), tys);
      if (List.for_all2((a, b) => a === b, tys, tys')) {
        ty;
      } else {
        Prod(tys') |> rewrap;
      };
    | TupLabel(label, t) =>
      let label' = subst(s, x, label);
      let t' = subst(s, x, t);
      if (label' === label && t' === t) {
        ty;
      } else {
        TupLabel(label', t') |> rewrap;
      };
    | Sum(sm) =>
      let sm' = ConstructorMap.map(Option.map(subst(s, x)), sm);
      if (sm' === sm) {
        ty;
      } else {
        Sum(sm') |> rewrap;
      };
    | Poly(tp2, _) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) => ty
    | Poly(tp2, t) =>
      let t' = subst(s, x, t);
      if (t' === t) {
        ty;
      } else {
        Poly(tp2, t') |> rewrap;
      };
    | Rec(tp2, _) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) => ty
    | Rec(tp2, t) =>
      let t' = subst(s, x, t);
      if (t' === t) {
        ty;
      } else {
        Rec(tp2, t') |> rewrap;
      };
    | List(t) =>
      let t' = subst(s, x, t);
      if (t' === t) {
        ty;
      } else {
        List(t') |> rewrap;
      };
    | Var(y) => str == y ? s : ty
    | Parens(t) =>
      let t' = subst(s, x, t);
      if (t' === t) {
        ty;
      } else {
        Parens(t') |> rewrap;
      };
    | Projector(data, t) =>
      let t' = subst(s, x, t);
      if (t' === t) {
        ty;
      } else {
        Projector(data, t') |> rewrap;
      };
    | ProdProjection(t1, t2) =>
      let t1' = subst(s, x, t1);
      let t2' = subst(s, x, t2);
      if (t1' === t1 && t2' === t2) {
        ty;
      } else {
        ProdProjection(t1', t2') |> rewrap;
      };
    | ProdExtension(t1, t2) =>
      let t1' = subst(s, x, t1);
      let t2' = subst(s, x, t2);
      if (t1' === t1 && t2' === t2) {
        ty;
      } else {
        ProdExtension(t1', t2') |> rewrap;
      };
    };
  | None => ty
  };
};

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) => subst(ty, tp, ty_body)
  | _ => ty
  };

/* Type Equality: This coincides with alpha equivalence for normalized types.
   Other types may be equivalent but this will not detect so if they are not normalized. */
let fast_equal = Equality.semantic.typ;
let equal = (t1: t, t2: t): bool => Equality.syntactic.typ(t1, t2);

let project_type = (tys: list(t), label: string): option(t) =>
  switch (LabeledTuple.find_label(match_tup_label, tys, label)) {
  | Some({term: TupLabel(_, ty), _}) => Some(ty)
  | _ => None
  };

let product_extension = (tys1: list(t), tys2: list(t)): term => {
  let get_lv = (t: t) => {
    switch (match_tup_label(t)) {
    | Some((l, t)) => (Some(l), t)
    | None => (None, t)
    };
  };

  let new_tys =
    LabeledTuple.extension(List.map(get_lv, tys1), List.map(get_lv, tys2))
    |> List.map(((l, ty)) =>
         switch (l) {
         | Some(l) => TupLabel(fresh(Label(l)), ty) |> temp
         | None => ty
         }
       );
  Prod(new_tys);
};

/**
 * Removes duplicate labels from a given list of types inside a tuple.
 *
 * For each label in the list of duplicate labels, keeps only the first occurrence,
 * replacing its type with Unknown(Internal), and removes all subsequent occurrences.
 *
 * @param duplicate_labels - The list of duplicate labels.
 * @param tys - The list of types to remove duplicates from.
 * @return A new list of types where, for each duplicate label, only the first occurrence
 *         is kept (with type Unknown(Internal)), and all subsequent occurrences are removed.
 */
let remove_duplicate_labels =
    (~duplicate_labels: list(LabeledTuple.label), tys: list(t)): list(t) => {
  let (_, rev_deduplicated) =
    List.fold_left(
      ((seen_duplicates, rev_deduplicated_types), ty) => {
        let tup_label = match_tup_label(ty);
        switch (tup_label) {
        | Some((l, _))
            when
              List.mem(l, duplicate_labels) && List.mem(l, seen_duplicates) => (
            seen_duplicates,
            rev_deduplicated_types,
          )
        | Some((l, _)) when List.mem(l, duplicate_labels) => (
            [l, ...seen_duplicates],
            [
              TupLabel(Label(l) |> temp, Unknown(Internal) |> temp) |> temp,
              ...rev_deduplicated_types,
            ],
          )
        | Some(_) => (seen_duplicates, [ty, ...rev_deduplicated_types])
        | None => (seen_duplicates, [ty, ...rev_deduplicated_types])
        };
      },
      ([], []),
      tys,
    );
  List.rev(rev_deduplicated);
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
  | TupLabel({term: ExplicitNonlabel, _}, ty) =>
    weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty)
  | ProdProjection(ty, label) =>
    let (_, rewrap) = unwrap(ty);

    let normalized_ty =
      weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty);

    (
      switch (normalized_ty.term, label.term) {
      | (Prod(tys), Label(l)) => project_type(tys, l)
      | _ => None // It would be better to do this via a more direct error recovery mechanism in statics
      }
    )
    |> Option.value(~default=Unknown(Internal) |> rewrap);
  | Prod(ts) =>
    let (_, rewrap) = unwrap(ty);
    let duplicate_labels =
      LabeledTuple.get_duplicate_labels(match_tup_label, ts);
    if (List.is_empty(duplicate_labels)) {
      ty;
    } else {
      let cleaned_ts = remove_duplicate_labels(~duplicate_labels, ts);
      Prod(cleaned_ts) |> rewrap;
    };
  | ProdExtension(t1, t2) =>
    let (_, rewrap) = unwrap(ty);

    let t1 = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t1);
    let t2 = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t2);
    switch (t1.term, t2.term) {
    | (Prod(tys1), Prod(tys2)) => product_extension(tys1, tys2) |> rewrap
    | _ =>
      // It would be better to do this via a more direct error recovery mechanism in statics
      Unknown(Internal) |> rewrap
    };
  | _ => ty
  };
};

/* Normalize profiling counters */
let normalize_calls = ref(0);
let normalize_total_ms = ref(0.0);
let normalize_depth = ref(0);

let reset_normalize_stats = () => {
  normalize_calls := 0;
  normalize_total_ms := 0.0;
  normalize_depth := 0;
};

let print_normalize_stats = () => {
  Printf.printf("[NORM] normalize: %d calls\n%!", normalize_calls^);
};

let rec normalize = (~rec_counter=0, ctx: Ctx.t, ty: t): t => {
  normalize_calls := normalize_calls^ + 1;
  let is_top = normalize_depth^ == 0;
  let start =
    if (is_top) {
      JsUtil.precise_timestamp();
    } else {
      0.0;
    };
  normalize_depth := normalize_depth^ + 1;
  let result = {
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
    | ExplicitNonlabel
    | Label(_) => ty
    | Parens(t)
    | Projector(_, t) => normalize(ctx, t)
    | List(t) =>
      let t' = normalize(ctx, t);
      t === t' ? ty : List(t') |> rewrap;
    | Arrow(t1, t2) =>
      let t1' = normalize(ctx, t1);
      let t2' = normalize(ctx, t2);
      t1 === t1' && t2 === t2' ? ty : Arrow(t1', t2') |> rewrap;
    | Prod(ts) =>
      let ts' = List.map(normalize(ctx), ts);
      List.for_all2((===), ts, ts') ? ty : Prod(ts') |> rewrap;
    | ProdProjection(_) => weak_head_normalize(ctx, ty) |> normalize(ctx)
    | ProdExtension(_) => weak_head_normalize(ctx, ty) |> normalize(ctx)
    | TupLabel({term: ExplicitNonlabel, _}, ty) => normalize(ctx, ty)
    | TupLabel(label, t) =>
      let label' = normalize(ctx, label);
      let t' = normalize(ctx, t);
      label === label' && t === t' ? ty : TupLabel(label', t') |> rewrap;
    | Sum(ts) =>
      let ts' = ConstructorMap.map(Option.map(normalize(ctx)), ts);
      ts === ts' ? ty : Sum(ts') |> rewrap;
    | Rec(tpat, t) =>
      let t' = normalize(Ctx.extend_dummy_tvar(ctx, tpat), t);
      t === t' ? ty : Rec(tpat, t') |> rewrap;
    | Poly(name, t) =>
      let t' = normalize(Ctx.extend_dummy_tvar(ctx, name), t);
      t === t' ? ty : Poly(name, t') |> rewrap;
    | ProofOf(_) => ty
    };
  };
  normalize_depth := normalize_depth^ - 1;
  if (is_top) {
    normalize_total_ms :=
      normalize_total_ms^ +. (JsUtil.precise_timestamp() -. start);
  };
  result;
};

/* Performance counters for meet */
let meet_calls = ref(0);
let meet_sum_calls = ref(0);
let meet_sum_time_ms = ref(0.0);
let meet_var_eq = ref(0);
let meet_rec_rec = ref(0);
let meet_in_rec = ref(false);
let meet_sum_from_rec = ref(0);
let meet_phys_eq = ref(0);
let meet_var_expand = ref(0);
let meet_unknown = ref(0);
let reset_meet_stats = () => {
  meet_calls := 0;
  meet_sum_calls := 0;
  meet_sum_time_ms := 0.0;
  meet_var_eq := 0;
  meet_rec_rec := 0;
  meet_in_rec := false;
  meet_sum_from_rec := 0;
  meet_phys_eq := 0;
  meet_var_expand := 0;
  meet_unknown := 0;
};

/* Lattice meet on types. This was called 'join' in the 2019 Hazelnut live paper,
   but we're now calling it 'meet' to clarify that Unknown represents the top
   (least precise) element in the precision ordering: specific types dominate Unknown. */
let rec meet = (ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
  incr(meet_calls);
  if (ty1 === ty2) {
    incr(meet_phys_eq);
    Some(ty1);
  } else {
    let meet' = meet(ctx);
    switch (term_of(ty1), term_of(ty2)) {
    | (_, Parens(ty2))
    | (_, Projector(_, ty2)) => meet'(ty1, ty2)
    | (Parens(ty1), _)
    | (Projector(_, ty1), _) => meet'(ty1, ty2)
    | (TupLabel({term: ExplicitNonlabel, _}, ty1'), _) => meet'(ty1', ty2)
    | (_, TupLabel({term: ExplicitNonlabel, _}, ty2')) => meet'(ty1, ty2')
    | (Unknown(p1), Unknown(p2)) =>
      incr(meet_unknown);
      if (p1 == p2) {
        Some(ty1);
      } else {
        Some(Unknown(meet_type_provenance(p1, p2)) |> temp);
      };
    | (Unknown(_), _) =>
      incr(meet_unknown);
      Some(ty2);
    | (_, Unknown(_)) =>
      incr(meet_unknown);
      Some(ty1);
    | (Var(n1), Var(n2)) =>
      if (n1 == n2) {
        incr(meet_var_eq);
        Some(ty1);
      } else {
        let ty1' = Ctx.lookup_alias(ctx, n1);
        let ty2' = Ctx.lookup_alias(ctx, n2);
        switch (ty1', ty2') {
        | (Some(ty1), Some(ty2)) => meet'(ty1, ty2)
        | (Some(ty1), None) => meet'(ty1, ty2)
        | (None, Some(ty2)) => meet'(ty1, ty2)
        | (None, None) => None
        };
      }
    /* Var-Rec fast path: when a Var resolves to a Rec with the same
       tpat name as the Rec we're meeting with, they're the same recursive
       type — return the compact Var form. This avoids expensive structural
       comparison of the bodies (which may differ syntactically due to
       unrolling but are semantically equivalent). The lookup_alias call
       serves as a soundness check: it verifies the Var actually resolves
       to a type alias in the current context. */
    | (Var(name), Rec(tp2, _)) =>
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(rec_name) when rec_name == name =>
        switch (Ctx.lookup_alias(ctx, name)) {
        | Some({term: Rec(tp1, _), _})
            when TPat.tyvar_of_utpat(tp1) == Some(name) =>
          incr(meet_var_eq);
          Some(ty1)
        | _ =>
          /* Var resolves to something other than a matching Rec;
             fall through to general Var expansion */
          incr(meet_var_expand);
          let* ty_name = Ctx.lookup_alias(ctx, name);
          let+ ty_meet = meet'(ty_name, ty2);
          equal(ty_name, ty_meet) ? ty1 : ty_meet;
        }
      | _ =>
        incr(meet_var_expand);
        let* ty_name = Ctx.lookup_alias(ctx, name);
        let+ ty_meet = meet'(ty_name, ty2);
        equal(ty_name, ty_meet) ? ty1 : ty_meet;
      }
    | (Rec(tp1, _), Var(name)) =>
      switch (TPat.tyvar_of_utpat(tp1)) {
      | Some(rec_name) when rec_name == name =>
        switch (Ctx.lookup_alias(ctx, name)) {
        | Some({term: Rec(tp2, _), _})
            when TPat.tyvar_of_utpat(tp2) == Some(name) =>
          incr(meet_var_eq);
          Some(ty2)
        | _ =>
          incr(meet_var_expand);
          let* ty_name = Ctx.lookup_alias(ctx, name);
          let+ ty_meet = meet'(ty_name, ty1);
          equal(ty_name, ty_meet) ? ty2 : ty_meet;
        }
      | _ =>
        incr(meet_var_expand);
        let* ty_name = Ctx.lookup_alias(ctx, name);
        let+ ty_meet = meet'(ty_name, ty1);
        equal(ty_name, ty_meet) ? ty2 : ty_meet;
      }
    | (Var(name), _) =>
      incr(meet_var_expand);
      let* ty_name = Ctx.lookup_alias(ctx, name);
      let+ ty_meet = meet'(ty_name, ty2);
      equal(ty_name, ty_meet) ? ty1 : ty_meet;
    | (_, Var(name)) =>
      incr(meet_var_expand);
      let* ty_name = Ctx.lookup_alias(ctx, name);
      let+ ty_meet = meet'(ty_name, ty1);
      equal(ty_name, ty_meet) ? ty2 : ty_meet;
    /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
    | (ProdProjection(_), _) => meet'(weak_head_normalize(ctx, ty1), ty2)
    | (_, ProdProjection(_)) => meet'(ty1, weak_head_normalize(ctx, ty2))
    | (ProdExtension(_), _) => meet'(weak_head_normalize(ctx, ty1), ty2)
    | (_, ProdExtension(_)) => meet'(ty1, weak_head_normalize(ctx, ty2))
    | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
      incr(meet_rec_rec);
      let was_in_rec = meet_in_rec^;
      meet_in_rec := true;
      let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
      let ty1' =
        switch (TPat.tyvar_of_utpat(tp1), TPat.tyvar_of_utpat(tp2)) {
        | (Some(x1), Some(x2)) when x1 == x2 => ty1 /* Same var name, skip subst */
        | (_, Some(x2)) => subst(Var(x2) |> temp, tp1, ty1)
        | (_, None) => ty1
        };
      let result = {
        let+ ty_body = meet(ctx, ty1', ty2);
        Rec(tp1, ty_body) |> temp;
      };
      meet_in_rec := was_in_rec;
      result;
    | (Rec(_), _) => None
    | (Poly(x1, ty1), Poly(x2, ty2)) =>
      let ty1' =
        switch (TPat.tyvar_of_utpat(x2)) {
        | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
        | None => ty1
        };
      let ctx = Ctx.extend_dummy_tvar(ctx, x2);
      let+ ty_body = meet(ctx, ty1', ty2);
      Poly(x2, ty_body) |> temp;
    /* Note for above: there is no danger of free variable capture as
       subst itself performs capture avoiding substitution. However this
       may generate internal type variable names that in corner cases can
       be exposed to the user. We preserve the variable name of the
       second type to preserve synthesized type variable names, which
       come from user annotations. */
    | (Poly(_), _) => None
    | (Atom(c1), Atom(c2)) when c1 == c2 => Some(ty1)
    | (Atom(_), _) => None
    | (Label(_), Label("")) => Some(ty1)
    | (Label(""), Label(_)) => Some(ty2)
    | (Label(name1), Label(name2))
        when LabeledTuple.match_labels(name1, name2) =>
      Some(ty1)
    | (Label(_), _) => None
    | (Arrow(a1, a2), Arrow(b1, b2)) =>
      let* r1 = meet'(a1, b1);
      let+ r2 = meet'(a2, b2);
      if (r1 === a1 && r2 === a2) {
        ty1;
      } else if (r1 === b1 && r2 === b2) {
        ty2;
      } else {
        Grammar.Arrow(r1, r2) |> temp;
      };
    | (Arrow(_), _) => None
    | (TupLabel(la, a), TupLabel(lb, b)) =>
      let* rl = meet'(la, lb);
      let+ r = meet'(a, b);
      if (rl === la && r === a) {
        ty1;
      } else if (rl === lb && r === b) {
        ty2;
      } else {
        Grammar.TupLabel(rl, r) |> temp;
      };
    | (TupLabel(_), _) => None
    | (Prod(tys1), Prod(tys2)) =>
      let tys1 =
        remove_duplicate_labels(
          ~duplicate_labels=
            LabeledTuple.get_duplicate_labels(match_tup_label, tys1),
          tys1,
        );
      let tys2 =
        remove_duplicate_labels(
          ~duplicate_labels=
            LabeledTuple.get_duplicate_labels(match_tup_label, tys2),
          tys2,
        );

      if (List.length(tys1) != List.length(tys2)) {
        None;
      } else {
        let* tys = ListUtil.map2_opt(meet', tys1, tys2);
        let+ tys = OptUtil.sequence(tys);
        Prod(tys) |> temp;
      };
    | (Prod(_), _) => None
    | (Sum(sm1), Sum(sm2)) =>
      incr(meet_sum_calls);
      if (meet_in_rec^) {
        incr(meet_sum_from_rec);
      };
      let start = JsUtil.precise_timestamp();
      let result = {
        let+ sm' = ConstructorMap.meet(equal, meet(ctx), sm1, sm2);
        Sum(sm') |> temp;
      };
      meet_sum_time_ms :=
        meet_sum_time_ms^ +. (JsUtil.precise_timestamp() -. start);
      result;
    | (Sum(_), _) => None
    | (List(a), List(b)) =>
      let+ r = meet'(a, b);
      if (r === a) {
        ty1;
      } else if (r === b) {
        ty2;
      } else {
        Grammar.List(r) |> temp;
      };
    | (List(_), _) => None
    | (ProofOf(e1), ProofOf(e2)) =>
      Equality.semantic.exp(e1, e2) ? Some(ty1) : None
    | (ProofOf(_), _) => None
    // We would prefer for this to be a sort difference and never appear in a meet.
    // These get marked in statics but that does not remove them from the utyp's propagated on parents.
    | (ExplicitNonlabel, _) => None
    };
  };
};

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by matching against t2 */
let rec match_synswitch = (t1: t, t2: t) => {
  let (term1, rewrap1) = unwrap(t1);
  switch (term1, term_of(t2)) {
  | (Parens(t1), _) => Parens(match_synswitch(t1, t2)) |> rewrap1
  | (Projector(data, t1), _) =>
    Projector(data, match_synswitch(t1, t2)) |> rewrap1
  | (Unknown(SynSwitch), _) => t2
  // These cases can't have a synswitch inside
  | (Unknown(_), _)
  | (Atom(_), _)
  | (Label(_), _)
  | (ExplicitNonlabel, _)
  | (Var(_), _)
  | (Rec(_), _)
  | (ProofOf(_), _)
  | (ProdProjection(_), _)
  | (ProdExtension(_), _) => t1
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
  // HACK[Matt]: The only possible poly is `Poly Syn -> Syn`
  | (Poly(_), Poly(_)) => t2
  | (Poly(_), _) => t1
  };
};

let meet_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => OptUtil.and_then(meet(ctx, ty), acc),
    Some(empty),
    ts,
  );

let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
  meet(ctx, ty1, ty2) != None;

/**
   * Determines if one type (`ty1`) is more precise than another type (`ty2`) within a given context (`ctx`).
   *
   * @return - `true` if `ty1` is more precise than `ty2`, otherwise `false`.
   */
let is_more_precise = (ctx: Ctx.t, ty1: t, ty2: t): bool => {
  let met = meet(ctx, ty1, ty2);
  switch (met) {
  | None => false
  | Some(met) => Equality.semantic.typ(met, ty1)
  };
};

let rec matched_arrow_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_arrow_strict(ctx, ty)
  | Arrow(ty_in, ty_out) => Some((ty_in, ty_out))
  | Unknown(SynSwitch) =>
    Some((Unknown(SynSwitch) |> temp, Unknown(SynSwitch) |> temp))
  | _ => None
  };

let matched_arrow = (ctx, ty) =>
  matched_arrow_strict(ctx, ty)
  |> Option.value(
       ~default=(Unknown(Internal) |> temp, Unknown(Internal) |> temp),
     );

let rec matched_poly_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_poly_strict(ctx, ty)
  | Poly(t, ty) => Some((Some(t), ty))
  | Unknown(SynSwitch) => Some((None, Unknown(SynSwitch) |> temp))
  | _ => None
  };

let matched_poly = (ctx, ty) =>
  matched_poly_strict(ctx, ty)
  |> Option.value(~default=(None, Unknown(Internal) |> temp));

let rec get_labels = (ctx, ty): list(option(string)) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
  | Parens(ty) => get_labels(ctx, ty)
  | Prod(tys) => List.map(x => Option.map(fst, match_tup_label(x)), tys)
  | _ => []
  };
};

let rec matched_prod_strict:
  type a.
    (Ctx.t, list(a), a => option((string, a)), t, (string, a) => a) =>
    (list(a), option(list(t))) =
  (ctx: Ctx.t, es, get_label_es, ty: t, constructor) => {
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) =>
      matched_prod_strict(ctx, es, get_label_es, ty, constructor)
    | Prod(tys: list(t)) =>
      if (List.length(es) != List.length(tys)) {
        (es, None);
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
        );
      }
    | Unknown(SynSwitch) => (
        es,
        Some(List.init(List.length(es), _ => Unknown(SynSwitch) |> temp)),
      )
    | _ => (es, None)
    };
  };

let matched_prod = (ctx, es, get_label_es, ty, constructor) => {
  let (es, tys_opt) =
    matched_prod_strict(ctx, es, get_label_es, ty, constructor);
  (
    es,
    tys_opt
    |> Option.value(
         ~default=List.init(List.length(es), _ => Unknown(Internal) |> temp),
       ),
  );
};

let rec matched_list_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_list_strict(ctx, ty)
  | List(ty) => Some(ty)
  | Unknown(SynSwitch) => Some(Unknown(SynSwitch) |> temp)
  | _ => None
  };

let matched_list = (ctx, ty) =>
  matched_list_strict(ctx, ty)
  |> Option.value(~default=Unknown(Internal) |> temp);

let rec matched_args_strict = (ctx, ty, arity): Either.t('a, int) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_args_strict(ctx, ty, arity)
  | Prod(tys) when List.length(tys) == arity => L(tys)
  | Prod(tys) => R(List.length(tys))
  | _ when arity == 1 => L([ty])
  | Unknown(_) => L(List.init(arity, _ => Unknown(Internal) |> temp))
  | _ => R(1)
  };
};

let matched_label = (ctx, ty): option((t, t)) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | TupLabel({term: Label(ml), _}, ty) => Some((Label(ml) |> temp, ty))
  | Unknown(SynSwitch) =>
    Some((Unknown(SynSwitch) |> temp, Unknown(SynSwitch) |> temp))
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
  | Parens(x)
  | Projector(_, x) => is_syn(x)
  | Unknown(SynSwitch) => true
  | Unknown(_)
  | Atom(_)
  | Label(_)
  | Var(_)
  | Rec(_)
  | Poly(_)
  | ProofOf(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | Sum(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | ExplicitNonlabel => false
  };

let rec is_ana_atom = (ty: t) =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x)
  | Projector(_, x) => is_ana_atom(x)
  | Atom(a) => Some(a)
  | Unknown(_)
  | ExplicitNonlabel
  | Label(_)
  | Var(_)
  | Rec(_)
  | Poly(_)
  | ProofOf(_)
  | List(_)
  | Arrow(_)
  | Prod(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | Sum(_) => None
  };

let rec is_syn_plus = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x)
  | Projector(_, x) => is_syn_plus(x)
  | Unknown(SynSwitch) => true
  | Arrow(t1, t2) => is_syn(t1) && is_syn_plus(t2)
  | Poly(_, t) => is_syn(t)
  | ProofOf(_)
  | Unknown(_)
  | Atom(_)
  | ExplicitNonlabel
  | Label(_)
  | Var(_)
  | Rec(_)
  | List(_)
  | Prod(_)
  | Sum(_)
  | ProdProjection(_)
  | ProdExtension(_) => false
  };

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(ty)
  | Projector(_, ty) => needs_parens(ty)
  | Unknown(_)
  | Atom(_)
  | ExplicitNonlabel
  | Label(_)
  | List(_) /* is already wrapped in [] */
  | ProofOf(_)
  | Var(_) => false
  | ProdProjection(_, _)
  | ProdExtension(_, _)
  | TupLabel(_, _)
  | Rec(_, _)
  | Poly(_, _)
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
  | Parens(ty)
  | Projector(_, ty) => pretty_print(ty)
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
  | ProdProjection(t, label) =>
    pretty_print(t) ++ "." ++ pretty_print(label)
  | ProdExtension(t, label) =>
    pretty_print(t) ++ " + " ++ pretty_print(label)
  | Label(name) => name
  | ExplicitNonlabel => "_"
  | TupLabel(label, t) => pretty_print(label) ++ "=" ++ pretty_print(t)
  | Rec(tv, t) =>
    "rec " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | Poly(tv, t) =>
    "poly " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | ProofOf(_e) => "yes <e> indeed"
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

/* Replaces rec types with a variable with the same name as
 * their rec parameter. Intended mostly for printing */
let abstract_rec_types =
  map_term(
    ~f_typ=
      (continue, t) =>
        switch (t.term) {
        | Rec({term: Var(name), _}, _) => {
            ...t,
            term: Var(name),
          }
        | _ => continue(t)
        },
    _,
  );

/**
 * Converts a list of types (`tys`) into a product type.
 *
 * @param tys - A list of types to be combined into a product type.
 * @return A product type representing the combination of the input types
 */
let to_product = (tys: list(t)): t => TempGrammar.Typ.(prod(tys));
