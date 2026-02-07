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
  | Rec
  | Poly
  | ProofOf
  | ProdProjection
  | ProdExtension
  | Sig;

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
    annotation: IdTagged.IdTag.temp(),
  };

let all_ids_temp = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) =>
      {
        term: exp.term,
        annotation: IdTagged.IdTag.temp(),
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
  | Sum(_) => Sum
  | Rec(_) => Rec
  | Poly(_) => Poly
  | ProofOf(_) => ProofOf
  | ProdProjection(_) => ProdProjection
  | ProdExtension(_) => ProdExtension
  | Sig(_) => Sig;

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
  | Rec => "Recursive type"
  | Poly => "Type quantifier"
  | ProofOf => "Proof type"
  | ProdProjection => "Tuple projection"
  | ProdExtension => "Tuple extension"
  | Sig => "Signature type";

let rec is_arrow = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
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
  | ProdExtension(_)
  | Sig(_) => false
  };
};

let is_atom = (ty: t): bool =>
  switch (ty.term) {
  | Atom(_) => true
  | ProofOf(_)
  | Parens(_)
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
  | ProdExtension(_)
  | Sig(_) => false
  };

let rec has_fun = (typ: t) =>
  switch (typ.term) {
  | Parens(typ)
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
  | Sig(_) => false
  };

let rec is_poly = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
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
  | ProdExtension(_)
  | Sig(_) => false
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
  | Parens(ty) => free_vars(~bound, ty)
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
  | Sig(_) => []
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
  | Poly({term: Var(x), _}, ty) =>
    vars(ty) |> List.filter((x': string) => x' != x)
  | Poly(_, ty) => vars(ty)
  | ProofOf(_) => []
  | ExplicitNonlabel
  | Label(_) => []
  | TupLabel(_, ty)
  | ProdProjection(ty, _) => vars(ty)
  | ProdExtension(ty1, ty2) => vars(ty1) @ vars(ty2)
  | Sig(_) => []
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
  | Parens(ty) => 1 + num_nodes(ty)
  | Poly(_, ty) => 1 + num_nodes(ty)
  | ExplicitNonlabel
  | Label(_) => 1
  | TupLabel(_, ty) => 1 + num_nodes(ty)
  | ProofOf(_) => 10 // TODO[Matt]: this is a hack to make sure that Yes types are not counted as small
  | ProdProjection(ty1, ty2) => 1 + num_nodes(ty1) + num_nodes(ty2)
  | ProdExtension(ty1, ty2) => 1 + num_nodes(ty1) + num_nodes(ty2)
  | Sig(_) => 1
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
  | Poly(_, ty) => count_unknowns(ty)
  | ProofOf(_) => 0
  | ExplicitNonlabel
  | Label(_) => 0
  | TupLabel(_, ty) => count_unknowns(ty)
  | ProdProjection(ty1, _) => count_unknowns(ty1)
  | ProdExtension(ty1, ty2) => count_unknowns(ty1) + count_unknowns(ty2)
  | Sig(_) => 0
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
  | Poly(_, ty) => contains_sum_or_var(ty)
  | ProofOf(_) => false
  | ProdProjection(ty1, _) => contains_sum_or_var(ty1)
  | ProdExtension(ty1, ty2) =>
    contains_sum_or_var(ty1) || contains_sum_or_var(ty2)
  | ExplicitNonlabel
  | Label(_) => false
  | TupLabel(_, ty) => contains_sum_or_var(ty)
  | Sig(_) => false
  };

let rec subst = (s: t, x: TPat.t, ty: t): t => {
  switch (TPat.tyvar_of_utpat(x)) {
  | Some(str) =>
    let (term, rewrap) = Grammar.Annotated.unwrap(ty);
    switch (term) {
    | Atom(_) => ty
    | Label(name) => Grammar.Label(name) |> rewrap
    | ExplicitNonlabel => ExplicitNonlabel |> rewrap
    | Unknown(prov) => Unknown(prov) |> rewrap
    | Arrow(ty1, ty2) =>
      Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
    | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
    | TupLabel(label, ty) => TupLabel(label, subst(s, x, ty)) |> rewrap
    | Sum(sm) =>
      Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
    | Poly(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      Poly(tp2, ty) |> rewrap
    | Poly(tp2, ty) => Poly(tp2, subst(s, x, ty)) |> rewrap
    | Rec(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      Rec(tp2, ty) |> rewrap
    | Rec(tp2, ty) => Rec(tp2, subst(s, x, ty)) |> rewrap
    | List(ty) => List(subst(s, x, ty)) |> rewrap
    | Var(y) => str == y ? s : Var(y) |> rewrap
    | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
    | ProdProjection(t1, t2) =>
      ProdProjection(subst(s, x, t1), subst(s, x, t2)) |> rewrap
    | ProdExtension(t1, t2) =>
      ProdExtension(subst(s, x, t1), subst(s, x, t2)) |> rewrap
    | ProofOf(e) => ProofOf(e) |> rewrap
    | Sig(_) => ty
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
  | ExplicitNonlabel
  | Label(_) => ty
  | Parens(t) => normalize(ctx, t)
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(normalize(ctx), ts)) |> rewrap
  | ProdProjection(_) => weak_head_normalize(ctx, ty) |> normalize(ctx)
  | ProdExtension(_) => weak_head_normalize(ctx, ty) |> normalize(ctx)
  | TupLabel({term: ExplicitNonlabel, _}, ty) => normalize(ctx, ty) // Drop ExplicitNonlabel in normalization
  | TupLabel(label, ty) =>
    TupLabel(normalize(ctx, label), normalize(ctx, ty)) |> rewrap
  | Sum(ts) =>
    Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts)) |> rewrap
  | Rec(tpat, ty) =>
    /* NOTE: Dummy tvar added has fake id but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(tpat, normalize(Ctx.extend_dummy_tvar(ctx, tpat), ty)) |> rewrap
  | Poly(name, ty) =>
    Poly(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
  | ProofOf(_) => ty // Todo: should we normalize this?
  | Sig(items) =>
    /* Desugar signature to labeled tuple type:
       { let x : Int; let y : Bool } => (x=Int, y=Bool)
       Type aliases (SigType) don't contribute to the exported type. */
    let fields =
      items
      |> List.filter_map((item: Sig.t) =>
           switch (item.term) {
           | SigLet(pat) =>
             /* Extract name and type from pattern.
                let x : T => name="x", typ=T
                let x     => name="x", typ=Unknown */
             switch (pat.term) {
             | Asc({term: Var(name), _}, typ) =>
               Some(
                 TupLabel(Label(name) |> temp, normalize(ctx, typ)) |> temp,
               )
             | Var(name) =>
               Some(
                 TupLabel(Label(name) |> temp, Unknown(Internal) |> temp)
                 |> temp,
               )
             | _ => None
             }
           | SigType(_, _)
           | Invalid(_)
           | EmptyHole
           | MultiHole(_) => None
           }
         );
    switch (fields) {
    | [] => Prod([]) |> rewrap
    | _ => normalize(ctx, Prod(fields) |> rewrap)
    };
  };
};

/* Targeted Sig desugaring: Only converts Sig nodes to Prod (labeled tuples),
   preserving Parens and everything else. Use this instead of normalize when
   you need to desugar Sig types without stripping Parens wrappers. */
let rec desugar_sig = (ctx: Ctx.t, ty: t): t => {
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Sig(items) =>
    let fields =
      items
      |> List.filter_map((item: Sig.t) =>
           switch (item.term) {
           | SigLet(pat) =>
             switch (pat.term) {
             | Asc({term: Var(name), _}, typ) =>
               Some(
                 TupLabel(Label(name) |> temp, desugar_sig(ctx, typ))
                 |> temp,
               )
             | Var(name) =>
               Some(
                 TupLabel(Label(name) |> temp, Unknown(Internal) |> temp)
                 |> temp,
               )
             | _ => None
             }
           | SigType(_, _)
           | Invalid(_)
           | EmptyHole
           | MultiHole(_) => None
           }
         );
    switch (fields) {
    | [] => Prod([]) |> rewrap
    | _ => Prod(fields) |> rewrap
    };
  | Parens(t) => Parens(desugar_sig(ctx, t)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(desugar_sig(ctx, t1), desugar_sig(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(desugar_sig(ctx), ts)) |> rewrap
  | List(t) => List(desugar_sig(ctx, t)) |> rewrap
  | TupLabel(label, ty) =>
    TupLabel(desugar_sig(ctx, label), desugar_sig(ctx, ty)) |> rewrap
  | _ => ty
  };
};

/* Lattice meet on types. This was called 'join' in the 2019 Hazelnut live paper,
   but we're now calling it 'meet' to clarify that Unknown represents the top
   (least precise) element in the precision ordering: specific types dominate Unknown. */
let rec meet = (ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
  let meet' = meet(ctx);
  switch (term_of(ty1), term_of(ty2)) {
  | (_, Parens(ty2)) => meet'(ty1, ty2)
  | (Parens(ty1), _) => meet'(ty1, ty2)
  | (TupLabel({term: ExplicitNonlabel, _}, ty1'), _) => meet'(ty1', ty2)
  | (_, TupLabel({term: ExplicitNonlabel, _}, ty2')) => meet'(ty1, ty2')
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(meet_type_provenance(p1, p2)) |> temp)
  | (Unknown(_), _) => Some(ty2)
  | (_, Unknown(_)) => Some(ty1)
  | (Var(n1), Var(n2)) =>
    if (n1 == n2) {
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
  | (Var(name), _) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_meet = meet'(ty_name, ty2);
    equal(ty_name, ty_meet) ? ty1 : ty_meet;
  | (_, Var(name)) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_meet = meet'(ty_name, ty1);
    equal(ty_name, ty_meet) ? ty2 : ty_meet;
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (ProdProjection(_), _) => meet'(weak_head_normalize(ctx, ty1), ty2)
  | (_, ProdProjection(_)) => meet'(ty1, weak_head_normalize(ctx, ty2))
  | (ProdExtension(_), _) => meet'(weak_head_normalize(ctx, ty1), ty2)
  | (_, ProdExtension(_)) => meet'(ty1, weak_head_normalize(ctx, ty2))
  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
      | None => ty1
      };
    let+ ty_body = meet(ctx, ty1', ty2);
    Rec(tp1, ty_body) |> temp;
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
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = meet'(ty1, ty1');
    let+ ty2 = meet'(ty2, ty2');
    Arrow(ty1, ty2) |> temp;
  | (Arrow(_), _) => None
  | (TupLabel(lab1, ty1'), TupLabel(lab2, ty2')) =>
    let* lab = meet'(lab1, lab2);
    let+ ty = meet'(ty1', ty2');
    TupLabel(lab, ty) |> temp;
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
    let+ sm' = ConstructorMap.meet(equal, meet(ctx), sm1, sm2);
    Sum(sm') |> temp;
  | (Sum(_), _) => None
  | (List(ty1), List(ty2)) =>
    let+ ty = meet'(ty1, ty2);
    List(ty) |> temp;
  | (List(_), _) => None
  | (ProofOf(e1), ProofOf(e2)) =>
    Equality.semantic.exp(e1, e2) ? Some(ty1) : None
  | (ProofOf(_), _) => None
  // We would prefer for this to be a sort difference and never appear in a meet.
  // These get marked in statics but that does not remove them from the utyp's propagated on parents.
  | (ExplicitNonlabel, _) => None
  | (Sig(_), _) => None
  };
};

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by matching against t2 */
let rec match_synswitch = (t1: t, t2: t) => {
  let (term1, rewrap1) = unwrap(t1);
  switch (term1, term_of(t2)) {
  | (Parens(t1), _) => Parens(match_synswitch(t1, t2)) |> rewrap1
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
  | (Sig(_), _) => t1
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
  | Parens(x) => is_syn(x)
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
  | ExplicitNonlabel
  | Sig(_) => false
  };

let rec is_ana_atom = (ty: t) =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_ana_atom(x)
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
  | Sum(_)
  | Sig(_) => None
  };

let rec is_syn_plus = (ty: t): bool =>
  switch (ty |> term_of) {
  | TupLabel(_, x)
  | Parens(x) => is_syn_plus(x)
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
  | ProdExtension(_)
  | Sig(_) => false
  };

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(ty) => needs_parens(ty)
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
  | Sig(_) => false /* already wrapped in {} */
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
  | Sig(items) =>
    let sig_item_str = (item: Sig.t) =>
      switch (item.term) {
      | SigLet(p) =>
        "let "
        ++ (
          switch (IdTagged.term_of(p)) {
          | Var(x) => x
          | Asc(p', t) =>
            (
              switch (IdTagged.term_of(p')) {
              | Var(x) => x
              | _ => "?"
              }
            )
            ++ " : "
            ++ pretty_print(t)
          | _ => "?"
          }
        )
      | SigType(tp, t) =>
        "type " ++ pretty_print_tvar(tp) ++ " = " ++ pretty_print(t)
      | EmptyHole => "?"
      | Invalid(s) => s
      | MultiHole(_) => "?"
      };
    "{ "
    ++ String.concat(
         "; ",
         List.map(sig_item_str, items),
       )
    ++ " }"
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
 * Converts a list of types (`tys`) into a product type.
 *
 * @param tys - A list of types to be combined into a product type.
 * @return A product type representing the combination of the input types
 */
let to_product = (tys: list(t)): t => TempGrammar.Typ.(prod(tys));
