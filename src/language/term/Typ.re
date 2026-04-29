open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Atom(Atom.cls)
  | DrvQuoteTy
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
  | TypLam
  | TypParamAp
  | TypTuple
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
  | DrvQuoteTy(_) => DrvQuoteTy
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
  | TypLam(_) => TypLam
  | TypParamAp(_) => TypParamAp
  | TypTuple(_) => TypTuple
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
  | DrvQuoteTy => "Derivation-Mode Quotation Type"
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
  | TypLam => "Type-level function"
  | TypParamAp => "Type parameter application"
  | TypTuple => "Type parameter argument tuple"
  | ProofOf => "Proof type"
  | ProdProjection => "Tuple projection"
  | ProdExtension => "Tuple extension"
  | Sig => "Signature type";

let rec is_arrow = (typ: t) => {
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ) => is_arrow(typ)
  | Arrow(_) => true
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | List(_)
  | Label(_)
  | ExplicitNonlabel
  | Prod(_)
  | Var(_)
  | TypLam(_)
  | TypParamAp(_)
  | TypTuple(_)
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
  | DrvQuoteTy(_)
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
  | TypLam(_)
  | TypParamAp(_)
  | TypTuple(_)
  | Rec(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | Sig(_) => false
  };

let rec has_fun = (typ: t) =>
  switch (typ.term) {
  | Parens(typ)
  | Projector(_, typ)
  | TupLabel(_, typ)
  | ProdProjection(typ, _) => has_fun(typ)
  | Arrow(_)
  | Poly(_)
  | TypLam(_)
  | ProofOf(_) => true
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel
  | Var(_) => false
  | TypParamAp(t1, t2) => has_fun(t1) || has_fun(t2)
  | TypTuple(ts) => List.exists(has_fun, ts)
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
  | Projector(_, typ)
  | TupLabel(_, typ) => is_poly(typ)
  | Poly(_) => true
  | ProofOf(_)
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Arrow(_)
  | List(_)
  | Label(_)
  | ExplicitNonlabel
  | Prod(_)
  | Var(_)
  | Sum(_)
  | TypLam(_)
  | TypParamAp(_)
  | TypTuple(_)
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

let add_source =
  List.map2((id, ty) =>
    {
      id,
      ty,
    }
  );

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
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel => []
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | TypParamAp(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | TypTuple(ts) => List.concat_map(free_vars(~bound), ts)
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
  | Poly(x, ty)
  | TypLam(x, ty) =>
    free_vars(~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound, ty)
  | ProofOf(_) => []
  | Sig(_) => []
  };

let rec vars = (ty: t): list(Var.t) =>
  switch (ty.term) {
  | Atom(_)
  | DrvQuoteTy(_) => []
  | Unknown(_) => []
  | Var(x) => [x]
  | TypParamAp(ty1, ty2) => vars(ty1) @ vars(ty2)
  | TypTuple(ts) => List.concat_map(vars, ts)
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
  | TypLam({term: Var(x), _}, ty) =>
    vars(ty) |> List.filter((x': string) => x' != x)
  | TypLam(_, ty) => vars(ty)
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
  | DrvQuoteTy(_)
  | Unknown(_) => 1
  | Var(_) => 1
  | TypParamAp(t1, t2) => 1 + num_nodes(t1) + num_nodes(t2)
  | TypTuple(ts) =>
    1 + List.fold_left((acc, ty) => acc + num_nodes(ty), 0, ts)
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
  | Poly(_, ty)
  | TypLam(_, ty) => 1 + num_nodes(ty)
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
  | DrvQuoteTy(_)
  | Var(_) => 0
  | TypParamAp(t1, t2) => count_unknowns(t1) + count_unknowns(t2)
  | TypTuple(ts) =>
    List.fold_left((acc, ty) => acc + count_unknowns(ty), 0, ts)
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
  | Poly(_, ty)
  | TypLam(_, ty) => count_unknowns(ty)
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
  | DrvQuoteTy(_)
  | Unknown(_) => false
  | Var(_)
  | TypParamAp(_, _)
  | Sum(_) => true
  | TypTuple(ts) => List.exists(contains_sum_or_var, ts)
  | Arrow(t1, t2) => contains_sum_or_var(t1) || contains_sum_or_var(t2)
  | Prod(tys) => List.exists(contains_sum_or_var, tys)
  | Rec(_, ty) => contains_sum_or_var(ty)
  | List(ty) => contains_sum_or_var(ty)
  | Parens(ty)
  | Projector(_, ty) => contains_sum_or_var(ty)
  | Poly(_, ty)
  | TypLam(_, ty) => contains_sum_or_var(ty)
  | ProofOf(_) => false
  | ProdProjection(ty1, _) => contains_sum_or_var(ty1)
  | ProdExtension(ty1, ty2) =>
    contains_sum_or_var(ty1) || contains_sum_or_var(ty2)
  | ExplicitNonlabel
  | Label(_) => false
  | TupLabel(_, ty) => contains_sum_or_var(ty)
  | Sig(_) => false
  };

/* Capture-avoiding substitution of `s` for `x` in `ty`.

   When recursing under a type binder `Poly(tp2, body)` or `Rec(tp2, body)`
   whose name occurs free in `s`, naive substitution would let occurrences of
   that name introduced by substituting `s` be captured by the binder. To
   avoid this, we alpha-rename the clashing binder to a fresh name (via
   `fresh_var`) before recursing. The inner subst used for renaming is itself
   capture-avoiding, so repeated collisions are handled naturally. */
let rec subst = (s: t, x: TPat.t, ty: t): t => {
  let avoid_capture = (tp2: TPat.t, body: t): (TPat.t, t) =>
    switch (TPat.tyvar_of_utpat(tp2)) {
    | Some(name) when List.mem(name, free_vars(s)) =>
      let fresh = fresh_var(name);
      let tp2': TPat.t = Var(fresh) |> TPat.fresh;
      let body' = subst(Var(fresh) |> temp, tp2, body);
      (tp2', body');
    | _ => (tp2, body)
    };
  switch (TPat.tyvar_of_utpat(x)) {
  | Some(str) =>
    let (term, rewrap) = Annotated.unwrap(ty);
    switch (term) {
    | Atom(_) => ty
    | Label(name) => Grammar.Label(name) |> rewrap
    | ExplicitNonlabel => ExplicitNonlabel |> rewrap
    | Unknown(prov) => Unknown(prov) |> rewrap
    | Arrow(ty1, ty2) =>
      Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
    | TypParamAp(ty1, ty2) =>
      TypParamAp(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
    | TypTuple(ts) => TypTuple(List.map(subst(s, x), ts)) |> rewrap
    | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
    | TupLabel(label, ty) => TupLabel(label, subst(s, x, ty)) |> rewrap
    | Sum(sm) =>
      Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
    | Poly(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      Poly(tp2, ty) |> rewrap
    | Poly(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      Poly(tp2', subst(s, x, ty')) |> rewrap;
    | TypLam(tp2, ty)
        when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      TypLam(tp2, ty) |> rewrap
    | TypLam(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      TypLam(tp2', subst(s, x, ty')) |> rewrap;
    | Rec(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
      Rec(tp2, ty) |> rewrap
    | Rec(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      Rec(tp2', subst(s, x, ty')) |> rewrap;
    | List(ty) => List(subst(s, x, ty)) |> rewrap
    | Var(y) => str == y ? s : Var(y) |> rewrap
    | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
    | Projector(data, ty) => Projector(data, subst(s, x, ty)) |> rewrap
    | ProdProjection(t1, t2) =>
      ProdProjection(subst(s, x, t1), subst(s, x, t2)) |> rewrap
    | ProdExtension(t1, t2) =>
      ProdExtension(subst(s, x, t1), subst(s, x, t2)) |> rewrap
    | ProofOf(e) => ProofOf(e) |> rewrap
    | Sig(_) => ty
    | DrvQuoteTy(_) => ty
    };
  | None => ty
  };
};

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) => subst(ty, tp, ty_body)
  | _ => ty
  };

/* One-step unrolling of a recursive type, including higher-kinded ones.

   - For `Rec(name, body)` (kind `*` — body has no top-level `TypLam`):
     standard unfold `body[Rec/name]`, same as `unroll`.
   - For `TypParamAp(Rec(name, TypLam(p, body)), arg)` (kind `*` —
     instantiation of a higher-kinded recursive family): substitute the
     whole `Rec` for `Var(name)` in the `TypLam` body, then β-reduce with
     `arg`. The result has `TypParamAp(Rec(name, TypLam(...)), …)` self-
     references at the same `Rec` (with possibly different arguments),
     which is the canonical encoding of the recursive family's
     specializations.

   This stops at exactly one level of unrolling and is safe for non-
   uniform recursion where the recursive type cannot be expressed as a
   finite kind-`*` `Rec(...)`. */
/* Apply a list of arguments one at a time to a curried `TypLam` chain.
   Used by `unfold_one` and TypParamAp reduction when the argument is a
   `TypTuple` bundling multiple args for a single source-level
   application like `Either(Int, Bool)`. */
let rec apply_args = (fn: t, args: list(t)): t =>
  switch (args) {
  | [] => fn
  | [arg, ...rest] =>
    switch (term_of(fn)) {
    | TypLam(p, body) => apply_args(subst(arg, p, body), rest)
    | _ =>
      /* Out of TypLams to peel; preserve the residual application. */
      switch (rest) {
      | [] => TypParamAp(fn, arg) |> temp
      | _ => TypParamAp(fn, TypTuple([arg, ...rest]) |> temp) |> temp
      }
    }
  };

let unfold_one = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, body) => subst(ty, tp, body)
  | TypParamAp(fn, arg) =>
    switch (term_of(fn)) {
    | Rec(tp, body) =>
      let unrolled = subst(fn, tp, body);
      switch (term_of(arg)) {
      | TypTuple(args) => apply_args(unrolled, args)
      | _ =>
        switch (term_of(unrolled)) {
        | TypLam(p, inner) => subst(arg, p, inner)
        | _ => TypParamAp(unrolled, arg) |> temp
        }
      };
    | _ => ty
    }
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
  | Parens(t)
  | Projector(_, t) =>
    weak_head_normalize(~rec_counter=rec_counter + 1, ctx, t)
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, ty)
    | None => ty
    }
  | TypParamAp(fn, arg) =>
    let (_, rewrap) = unwrap(ty);
    let fn_whnf = weak_head_normalize(~rec_counter=rec_counter + 1, ctx, fn);
    switch (fn_whnf.term, term_of(arg)) {
    | (TypLam(param, body), TypTuple([head, ...rest])) =>
      /* Multi-argument application against a curried TypLam chain:
         peel one TypLam at a time, consuming `head` first and re-wrapping
         the remainder if any args remain. */
      let body' = subst(head, param, body);
      switch (rest) {
      | [] => weak_head_normalize(~rec_counter=rec_counter + 1, ctx, body')
      | _ =>
        weak_head_normalize(
          ~rec_counter=rec_counter + 1,
          ctx,
          TypParamAp(body', TypTuple(rest) |> temp) |> rewrap,
        )
      };
    | (TypLam(param, body), TypTuple([])) =>
      /* Empty tuple as argument shouldn't occur in practice; treat as
         no-op application — return the body. */
      weak_head_normalize(~rec_counter=rec_counter + 1, ctx, body) |> ignore;
      TypLam(param, body) |> rewrap;
    | (TypLam(param, body), _) =>
      weak_head_normalize(
        ~rec_counter=rec_counter + 1,
        ctx,
        subst(arg, param, body),
      )
    | (Rec(_), _) =>
      /* `TypParamAp(Rec(name, body), arg)` is the canonical normal form for a
         higher-kinded recursive family applied at `arg`
         (i.e. `(μX:* → *. body)(arg)`). We do *not* push the application
         inside the `Rec` and β-reduce — doing so would expose the body's
         self-references `TypParamAp(Var(name), …)` to a binder that no longer
         wraps a `TypLam`, leaving them structurally ill-formed (see
         `Typ.unfold_one` for the one-step unrolling used by callers that
         need to peer inside, like `get_sum_constructors`). */
      TypParamAp(fn_whnf, arg) |> rewrap
    | (fn', _) => TypParamAp(fn' |> temp, arg) |> rewrap
    };
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
  | DrvQuoteTy(_)
  | ExplicitNonlabel
  | Label(_) => ty
  | Parens(t)
  | Projector(_, t) => normalize(ctx, t)
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | TypParamAp(t1, t2) =>
    let arg_normalized = normalize(ctx, t2);
    switch (weak_head_normalize(ctx, ty).term) {
    | TypParamAp({term: Rec(_), _} as fn_whnf, _) =>
      /* `TypParamAp(Rec(name, TypLam(p, body)), arg)` is the canonical
         normal form for a higher-kinded recursive family applied at
         `arg`. Don't unfold it — that would expand infinitely for
         non-uniform recursion (and produce ill-formed types if the
         body's `TypLam` were β-reduced through the `Rec`). The Rec's
         body is left in its original (typically `TypLam`-wrapped) form
         so that `unfold_one` can correctly substitute the recursive
         family for self-references when callers need to peer inside. */
      TypParamAp(fn_whnf, arg_normalized) |> rewrap
    | TypParamAp(_, _) => TypParamAp(normalize(ctx, t1), arg_normalized) |> rewrap
    | _ as whnf => normalize(ctx, whnf |> temp)
    };
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | TypTuple(ts) =>
    /* `TypTuple` is the multi-argument bundle in a type-level
       application; normalize each argument independently. It only
       appears as the second arg of a `TypParamAp`; its shape is preserved
       so kind checking can match it against the callee's tuple-arrow
       arity. */
    TypTuple(List.map(normalize(ctx), ts)) |> rewrap
  | Prod(ts) =>
    let ts = List.map(normalize(ctx), ts);
    let duplicate_labels =
      LabeledTuple.get_duplicate_labels(match_tup_label, ts);
    let ts =
      List.is_empty(duplicate_labels)
        ? ts : remove_duplicate_labels(~duplicate_labels, ts);
    Prod(ts) |> rewrap;
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
  | TypLam(name, ty) =>
    TypLam(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
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
  | Projector(_, t) => desugar_sig(ctx, t)
  | Arrow(t1, t2) =>
    Arrow(desugar_sig(ctx, t1), desugar_sig(ctx, t2)) |> rewrap
  | TypParamAp(t1, t2) =>
    TypParamAp(desugar_sig(ctx, t1), desugar_sig(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(desugar_sig(ctx), ts)) |> rewrap
  | List(t) => List(desugar_sig(ctx, t)) |> rewrap
  | TypLam(tp, t) => TypLam(tp, desugar_sig(ctx, t)) |> rewrap
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
  | (_, Parens(ty2))
  | (_, Projector(_, ty2)) => meet'(ty1, ty2)
  | (Parens(ty1), _)
  | (Projector(_, ty1), _) => meet'(ty1, ty2)
  | (TupLabel({term: ExplicitNonlabel, _}, ty1'), _) => meet'(ty1', ty2)
  | (_, TupLabel({term: ExplicitNonlabel, _}, ty2')) => meet'(ty1, ty2')
  | _ when Equality.syntactic.typ(ty1, ty2) => Some(ty1)
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
  | (
      TypParamAp({term: Rec(_), _} as r1, a1),
      TypParamAp({term: Rec(_), _} as r2, a2),
    ) =>
      /* Higher-kinded recursive families: meet structurally. The same
         `Rec` applied at the same argument is the same type; otherwise
         try unfolding both sides one step and re-meeting (this catches
         e.g. `(μX. λa. F)(Int)` ≡ `F[μX/X, Int/a]`). */
      switch (meet'(r1, r2), meet'(a1, a2)) {
      | (Some(r), Some(a)) => Some(TypParamAp(r, a) |> temp)
      | _ => meet'(unfold_one(ty1), unfold_one(ty2))
      }
  | (TypParamAp({term: Rec(_), _}, _), _) =>
    let unfolded = unfold_one(ty1);
    Equality.syntactic.typ(unfolded, ty1) ? None : meet'(unfolded, ty2);
  | (_, TypParamAp({term: Rec(_), _}, _)) =>
    let unfolded = unfold_one(ty2);
    Equality.syntactic.typ(unfolded, ty2) ? None : meet'(ty1, unfolded);
  | (TypParamAp(_), _) =>
    let ty1_whnf = weak_head_normalize(ctx, ty1);
    Equality.syntactic.typ(ty1_whnf, ty1) ? None : meet'(ty1_whnf, ty2);
  | (_, TypParamAp(_)) =>
    let ty2_whnf = weak_head_normalize(ctx, ty2);
    Equality.syntactic.typ(ty2_whnf, ty2) ? None : meet'(ty1, ty2_whnf);
  | (TypLam(x1, ty1), TypLam(x2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let ctx = Ctx.extend_dummy_tvar(ctx, x2);
    let+ ty_body = meet(ctx, ty1', ty2);
    TypLam(x2, ty_body) |> temp;
  | (TypLam(_), _) => None
  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
      | None => ty1
      };
    let+ ty_body = meet(ctx, ty1', ty2);
    Rec(tp1, ty_body) |> temp;
  | (Rec(_), _) =>
    /* A recursive type may meet a non-recursive form via one-step
       unrolling — e.g. `Rec(L, Sum[…, X])` meets the unrolled
       `Sum[…, Rec(L, Sum[…, X])]`. */
    let unfolded = unfold_one(ty1);
    Equality.syntactic.typ(unfolded, ty1) ? None : meet'(unfolded, ty2);
  | (_, Rec(_)) =>
    let unfolded = unfold_one(ty2);
    Equality.syntactic.typ(unfolded, ty2) ? None : meet'(ty1, unfolded);
  | (Poly(x1, ty1), Poly(x2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let ctx = Ctx.extend_dummy_tvar(ctx, x2);
    let+ ty_body = meet(ctx, ty1', ty2);
    Poly(x2, ty_body) |> temp;
  /* Note for above: `subst` is capture-avoiding (see its definition),
     so renaming `x1` to `x2` via substitution is safe. In rare cases
     where capture does trigger, `subst` generates fresh internal type
     variable names via `fresh_var` that may be exposed to the user. We
     preserve the variable name of the second type to preserve
     synthesized type variable names, which come from user annotations. */
  | (Poly(_), _) => None
  | (Atom(c1), Atom(c2)) when c1 == c2 => Some(ty1)
  | (Atom(_), _) => None
  | (DrvQuoteTy(d1), DrvQuoteTy(d2)) when d1 == d2 => Some(ty1)
  | (DrvQuoteTy(_), _) => None
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
  | (TypTuple(ts1), TypTuple(ts2))
      when List.length(ts1) == List.length(ts2) =>
    let* tys = ListUtil.map2_opt(meet', ts1, ts2);
    let+ tys = OptUtil.sequence(tys);
    TypTuple(tys) |> temp;
  | (TypTuple(_), _) => None
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
  | (Projector(data, t1), _) =>
    Projector(data, match_synswitch(t1, t2)) |> rewrap1
  | (Unknown(SynSwitch), _) => t2
  // These cases can't have a synswitch inside
  | (Unknown(_), _)
  | (Atom(_), _)
  | (DrvQuoteTy(_), _)
  | (Label(_), _)
  | (ExplicitNonlabel, _)
  | (Var(_), _)
  | (Rec(_), _)
  | (TypLam(_), _)
  | (ProofOf(_), _)
  | (ProdProjection(_), _)
  | (ProdExtension(_), _) => t1
  // These might
  | (List(ty1), List(ty2)) => List(match_synswitch(ty1, ty2)) |> rewrap1
  | (List(_), _) => t1
  | (TypParamAp(t1a, t1b), TypParamAp(t2a, t2b)) =>
    TypParamAp(match_synswitch(t1a, t2a), match_synswitch(t1b, t2b)) |> rewrap1
  | (TypParamAp(_), _) => t1
  | (TypTuple(ts1), TypTuple(ts2))
      when List.length(ts1) == List.length(ts2) =>
    TypTuple(List.map2(match_synswitch, ts1, ts2)) |> rewrap1
  | (TypTuple(_), _) => t1
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

let rec get_labels = (ctx, ty): list(option(string)) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
  | Parens(ty) => get_labels(ctx, ty)
  | Prod(tys) => List.map(x => Option.map(fst, match_tup_label(x)), tys)
  | _ => []
  };
};

let rec get_sum_constructors =
        (~rec_counter=0, ctx: Ctx.t, ty: t): option(sum_map) =>
  if (rec_counter > 1000) {
    None;
  } else {
    let ty = weak_head_normalize(ctx, ty);
    switch (term_of(ty)) {
    | Parens(ty) =>
      get_sum_constructors(~rec_counter=rec_counter + 1, ctx, ty)
    | Sum(sm) => Some(sm)
    | TypParamAp({term: Rec(_), _}, _) =>
      /* Higher-kinded recursive family applied at an argument; unfold one
         step and recurse. The unfolded result has `TypParamAp(Rec, …)` self-
         references at the (possibly different) recursive arguments,
         which `get_sum_constructors` would handle as another `TypParamAp(Rec, _)`
         if recursed into. */
      get_sum_constructors(~rec_counter=rec_counter + 1, ctx, unfold_one(ty))
    | Rec({term: Var(x), _}, ty_body) =>
      Ctx.is_alias(ctx, x)
        /* Monomorphic recursive alias: the alias name shadows the rec
           parameter, so peer inside without substituting the recursive
           type into payloads — payloads should mention the alias by
           name (e.g. `Var("Tree")`), not an eagerly expanded body. */
        ? get_sum_constructors(~rec_counter=rec_counter + 1, ctx, ty_body)
        : get_sum_constructors(
            ~rec_counter=rec_counter + 1,
            ctx,
            unfold_one(ty),
          )
    | Rec(_) =>
      get_sum_constructors(~rec_counter=rec_counter + 1, ctx, unfold_one(ty))
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
  | DrvQuoteTy(_)
  | Label(_)
  | Var(_)
  | Rec(_)
  | Poly(_)
  | TypLam(_)
  | TypParamAp(_)
  | TypTuple(_)
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
  | Parens(x)
  | Projector(_, x) => is_ana_atom(x)
  | Atom(a) => Some(a)
  | DrvQuoteTy(_)
  | Unknown(_)
  | ExplicitNonlabel
  | Label(_)
  | Var(_)
  | Rec(_)
  | Poly(_)
  | TypLam(_)
  | TypParamAp(_)
  | TypTuple(_)
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
  | Parens(x)
  | Projector(_, x) => is_syn_plus(x)
  | Unknown(SynSwitch) => true
  | Arrow(t1, t2) => is_syn(t1) && is_syn_plus(t2)
  | Poly(_, t) => is_syn(t)
  | TypLam(_, t) => is_syn(t)
  | ProofOf(_)
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | ExplicitNonlabel
  | Label(_)
  | Var(_)
  | Rec(_)
  | TypParamAp(_)
  | TypTuple(_)
  | List(_)
  | Prod(_)
  | Sum(_)
  | ProdProjection(_)
  | ProdExtension(_)
  | Sig(_) => false
  };

let rec is_arrow_like = (ty: t): bool =>
  switch (term_of(ty)) {
  | Unknown(_) => true
  | Arrow(_, _) => true
  | Poly(_, t) => is_arrow_like(t)
  | Parens(t)
  | Projector(_, t) => is_arrow_like(t)
  | _ => false
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
  | DrvQuoteTy(_)
  | List(_) /* is already wrapped in [] */
  | ProofOf(_)
  | Var(_) => false
  | ProdProjection(_, _)
  | ProdExtension(_, _)
  | TupLabel(_, _)
  | Rec(_, _)
  | Poly(_, _)
  | TypLam(_, _)
  | TypParamAp(_, _)
  | TypTuple(_) /* TypTuple is the bare argument bundle in `T(a, b)`; if
                   it ever appears outside a TypParamAp position we wrap so
                   downstream readers don't conflate it with a tuple. */
  | Arrow(_, _)
  | Prod(_)
  | Sum(_) => true /* disambiguate between (A + B) -> C and A + (B -> C) */
  | Sig(_) => false /* already wrapped in {} */
  };

let rec pretty_print_tvar = (tv: TPat.t): string =>
  switch (IdTagged.term_of(tv)) {
  | Var(x) => x
  | Param(head, params) =>
    pretty_print_tvar(head)
    ++ "("
    ++ String.concat(", ", List.map(pretty_print_tvar, params))
    ++ ")"
  | Tuple(tps) => String.concat(", ", List.map(pretty_print_tvar, tps))
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
  | DrvQuoteTy(d) => DrvSort.to_string(d)
  | Atom(Nat) => "Nat"
  | Atom(SInt) => "SInt"
  | Var(tvar) => tvar
  | TypLam(tv, t) =>
    "typfun " ++ pretty_print_tvar(tv) ++ " -> " ++ pretty_print(t)
  | TypParamAp(t1, t2) => pretty_print(t1) ++ "(" ++ pretty_print(t2) ++ ")"
  | TypTuple(ts) => String.concat(", ", List.map(pretty_print, ts))
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
    "{ " ++ String.concat("; ", List.map(sig_item_str, items)) ++ " }";
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
