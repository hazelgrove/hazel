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
  | TypFun
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
  | TypFun(_) => TypFun
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
  | TypFun => "Type-level function"
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
  | TypFun(_)
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
  | TypFun(_)
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
  | TypFun(_)
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
  | TypFun(_)
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
  | TypFun(x, ty) =>
    /* `x` may be a single binder or a `TPat.Tuple` with multiple
       binders; `tyvars_of` flattens both cases into the list of bound
       names. */
    free_vars(~bound=TPat.tyvars_of(x) @ bound, ty)
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
  | Rec(x, ty)
  | Poly(x, ty)
  | TypFun(x, ty) =>
    let bound = TPat.tyvars_of(x);
    vars(ty) |> List.filter((x': string) => !List.mem(x', bound));
  | List(ty) => vars(ty)
  | Parens(ty)
  | Projector(_, ty) => vars(ty)
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
  | TypFun(_, ty) => 1 + num_nodes(ty)
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
  | TypFun(_, ty) => count_unknowns(ty)
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
  | TypFun(_, ty) => contains_sum_or_var(ty)
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
  /* Rename a single binder to a fresh name in `body` if its name
     would capture a free variable of `s`. */
  let avoid_capture_one = (tp2: TPat.t, body: t): (TPat.t, t) =>
    switch (TPat.tyvar_of_utpat(tp2)) {
    | Some(name) when List.mem(name, free_vars(s)) =>
      let fresh = fresh_var(name);
      let tp2': TPat.t = Var(fresh) |> TPat.fresh;
      let body' = subst(Var(fresh) |> temp, tp2, body);
      (tp2', body');
    | _ => (tp2, body)
    };
  /* For a binder that may be a `TPat.Tuple([…])`, alpha-rename each
     element binder in turn so none captures free variables of `s`. A
     non-tuple binder is treated as a singleton list. */
  let avoid_capture = (tp2: TPat.t, body: t): (TPat.t, t) =>
    switch (tp2.term) {
    | Tuple(tps) =>
      let (tps', body') =
        List.fold_left(
          ((acc, b), tp) => {
            let (tp', b') = avoid_capture_one(tp, b);
            (acc @ [tp'], b');
          },
          ([], body),
          tps,
        );
      (
        {
          ...tp2,
          term: Tuple(tps'),
        },
        body',
      );
    | _ => avoid_capture_one(tp2, body)
    };
  /* `x` shadows the binder if it appears anywhere in the binder's
     flattened tyvars list. */
  let binder_shadows_x = (tp2: TPat.t): bool =>
    switch (TPat.tyvar_of_utpat(x)) {
    | Some(name) => List.mem(name, TPat.tyvars_of(tp2))
    | None => false
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
    | Poly(tp2, ty) when binder_shadows_x(tp2) => Poly(tp2, ty) |> rewrap
    | Poly(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      Poly(tp2', subst(s, x, ty')) |> rewrap;
    | TypFun(tp2, ty) when binder_shadows_x(tp2) =>
      TypFun(tp2, ty) |> rewrap
    | TypFun(tp2, ty) =>
      let (tp2', ty') = avoid_capture(tp2, ty);
      TypFun(tp2', subst(s, x, ty')) |> rewrap;
    | Rec(tp2, ty) when binder_shadows_x(tp2) => Rec(tp2, ty) |> rewrap
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

/* Substitute a list of types simultaneously for a list of binders.
   Used when reducing `TypAp(TypAbs(TPat.Tuple([a, b, …]), body, _),
   TypTuple([t1, t2, …]))` and the analogous `Poly` specialization in
   the statics: each `tk` is substituted for the corresponding binder
   in `body` in one step. The lists must have equal length; the
   caller is expected to enforce that. */
let subst_many = (args: list(t), binders: list(TPat.t), body: t): t =>
  List.fold_left2(
    (body, arg, binder) => subst(arg, binder, body),
    body,
    args,
    binders,
  );

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) => subst(ty, tp, ty_body)
  | _ => ty
  };

/* Apply a list of types to a `TypFun` callee.

   - Uncurried `TypFun(TPat.Tuple([a, b, …]), body)`: substitute the
     args element-wise in one step against the tuple's binders.
   - Single-binder `TypFun(p, body)`: peel one argument at a time.
   - Out of `TypFun`s to peel: preserve the residual application
     (`TypParamAp(fn, arg)` or `TypParamAp(fn, TypTuple(args))`). */
let rec apply_args = (fn: t, args: list(t)): t =>
  switch (args) {
  | [] => fn
  | _ =>
    switch (term_of(fn)) {
    | TypFun(p, body) =>
      let binders = TPat.binders_of(p);
      if (List.length(binders) > 1
          && List.length(binders) == List.length(args)) {
        subst_many(args, binders, body);
      } else {
        switch (args) {
        | [arg, ...rest] => apply_args(subst(arg, p, body), rest)
        | [] => fn
        };
      };
    | _ =>
      switch (args) {
      | [arg] => TypParamAp(fn, arg) |> temp
      | _ => TypParamAp(fn, TypTuple(args) |> temp) |> temp
      }
    }
  };

/* One-step unrolling of a (possibly higher-kinded) recursive type:

   - `Rec(name, body)` ⇒ `body[Rec/name]` (same as `unroll`).
   - `TypParamAp(Rec(name, TypFun(p, body)), arg)` ⇒ substitute the
     whole `Rec` for `Var(name)` in the `TypFun` body, then
     β-reduce with `arg`. The result has
     `TypParamAp(Rec(name, TypFun(...)), …)` self-references — the
     canonical encoding of the recursive family's specializations.

   Stops at exactly one unrolling, so non-uniform recursion is safe. */
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
        | TypFun(p, inner) => subst(arg, p, inner)
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
    | (TypFun(param, body), TypTuple(args))
        when
          List.length(TPat.binders_of(param)) > 1
          && List.length(TPat.binders_of(param)) == List.length(args) =>
      /* Multi-binder `TypFun` applied to a tuple of args of
         matching arity — substitute element-wise in one step. */
      weak_head_normalize(
        ~rec_counter=rec_counter + 1,
        ctx,
        subst_many(args, TPat.binders_of(param), body),
      )
    | (TypFun(param, body), TypTuple([head, ...rest])) =>
      /* Multi-argument application against a single-binder
         `TypFun`: consume one element at a time. */
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
    | (TypFun(param, body), TypTuple([])) =>
      /* Shouldn't occur in well-formed input; preserve as a no-op. */
      TypFun(param, body) |> rewrap
    | (TypFun(param, body), _) =>
      weak_head_normalize(
        ~rec_counter=rec_counter + 1,
        ctx,
        subst(arg, param, body),
      )
    | (Rec(_), _) =>
      /* `TypParamAp(Rec(name, TypFun(p, body)), arg)` is the
         canonical WHNF for a higher-kinded recursive family applied
         at `arg`. Unfolding one step lives in `unfold_one`; pushing
         the application into the `Rec` here would expose the body's
         self-references to a binder that no longer wraps a
         `TypFun`. */
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
      /* `TypParamAp(Rec, _)` is the WHNF for a higher-kinded
         recursive family applied at an argument; preserve it so we
         don't expand infinitely for non-uniform recursion.
         Callers needing to peer inside use `unfold_one`. */
      TypParamAp(fn_whnf, arg_normalized) |> rewrap
    | TypParamAp(_, _) =>
      TypParamAp(normalize(ctx, t1), arg_normalized) |> rewrap
    | _ as whnf => normalize(ctx, whnf |> temp)
    };
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | TypTuple(ts) =>
    /* `TypTuple` only appears as the second arg of a `TypParamAp`;
       preserve its shape so kind checking can match it against the
       callee's tuple-arrow arity. */
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
  | TypFun(name, ty) =>
    TypFun(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
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
                 |> temp
                 |> IdTagged.fast_copy(IdTagged.rep_id(item)),
               )
             | Var(name) =>
               Some(
                 TupLabel(Label(name) |> temp, Unknown(Internal) |> temp)
                 |> temp
                 |> IdTagged.fast_copy(IdTagged.rep_id(item)),
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
  | Projector(data, t) => Projector(data, desugar_sig(ctx, t)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(desugar_sig(ctx, t1), desugar_sig(ctx, t2)) |> rewrap
  | TypParamAp(t1, t2) =>
    TypParamAp(desugar_sig(ctx, t1), desugar_sig(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(desugar_sig(ctx), ts)) |> rewrap
  | List(t) => List(desugar_sig(ctx, t)) |> rewrap
  | TypFun(tp, t) => TypFun(tp, desugar_sig(ctx, t)) |> rewrap
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
  | (TypFun(x1, ty1), TypFun(x2, ty2)) =>
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let ctx = Ctx.extend_dummy_tvar(ctx, x2);
    let+ ty_body = meet(ctx, ty1', ty2);
    TypFun(x2, ty_body) |> temp;
  | (TypFun(_), _) => None
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
    /* A Poly binder is a single tpat that may itself be a `Tuple` of
       binder elements (multi-binder forms). To meet two `Poly`s, we
       require their binder lists to have the same arity, then
       alpha-rename ty1's binders to the names from x2. As a special
       case, a binder consisting entirely of holes (e.g. the
       `Poly(EmptyHole, _)` placeholder used as the analysis target
       for `TypAp` callees) is treated as a wildcard — it imposes no
       constraint on the other side's arity, and the meet keeps the
       other side's binder. */
    let bs1 = TPat.binders_of(x1);
    let bs2 = TPat.binders_of(x2);
    let is_wildcard = (bs: list(TPat.t)): bool =>
      List.for_all(
        (b: TPat.t) =>
          switch (b.term) {
          | EmptyHole
          | Invalid(_)
          | MultiHole(_) => true
          | _ => false
          },
        bs,
      );
    if (is_wildcard(bs1)) {
      let ctx = Ctx.extend_dummy_tvar(ctx, x2);
      let+ ty_body = meet(ctx, ty1, ty2);
      Poly(x2, ty_body) |> temp;
    } else if (is_wildcard(bs2)) {
      let ctx = Ctx.extend_dummy_tvar(ctx, x1);
      let+ ty_body = meet(ctx, ty1, ty2);
      Poly(x1, ty_body) |> temp;
    } else if (List.length(bs1) != List.length(bs2)) {
      None;
    } else {
      let ty1' =
        List.fold_left2(
          (body, b1, b2) =>
            switch (TPat.tyvar_of_utpat(b2)) {
            | Some(name) => subst(Var(name) |> temp, b1, body)
            | None => body
            },
          ty1,
          bs1,
          bs2,
        );
      let ctx = Ctx.extend_dummy_tvar(ctx, x2);
      let+ ty_body = meet(ctx, ty1', ty2);
      Poly(x2, ty_body) |> temp;
    };
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
  | (TypTuple(ts1), TypTuple(ts2)) when List.length(ts1) == List.length(ts2) =>
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
  | (TypFun(_), _)
  | (ProofOf(_), _)
  | (ProdProjection(_), _)
  | (ProdExtension(_), _) => t1
  // These might
  | (List(ty1), List(ty2)) => List(match_synswitch(ty1, ty2)) |> rewrap1
  | (List(_), _) => t1
  | (TypParamAp(t1a, t1b), TypParamAp(t2a, t2b)) =>
    TypParamAp(match_synswitch(t1a, t2a), match_synswitch(t1b, t2b))
    |> rewrap1
  | (TypParamAp(_), _) => t1
  | (TypTuple(ts1), TypTuple(ts2)) when List.length(ts1) == List.length(ts2) =>
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

// A sum answers a query naming it, however the query spells the name.
let is_askable = (ctx: Ctx.t, actual: t, query: t): bool =>
  is_consistent(ctx, actual, query)
  || (
    switch (
      term_of(weak_head_normalize(ctx, actual)),
      term_of(weak_head_normalize(ctx, query)),
    ) {
    | (Sum(_), Sum(_) | Var(_) | TypParamAp(_, _)) => true
    | _ => false
    }
  );

let gap: t = temp(Unknown(Hole(EmptyHole)));

let rec is_gap = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) => is_gap(inner)
  | Unknown(Internal)
  | Unknown(Hole(EmptyHole))
  | Unknown(SynSwitch) => true
  | _ => false
  };

let rec is_empty = (ty: t): bool =>
  if (is_gap(ty)) {
    true;
  } else {
    switch (term_of(ty)) {
    | List(inner)
    | TypFun(_, inner)
    | Parens(inner)
    | Projector(_, inner)
    | Rec(_, inner)
    | Poly(_, inner) => is_empty(inner)
    | Arrow(left, right)
    | TupLabel(left, right)
    | TypParamAp(left, right)
    | ProdProjection(left, right)
    | ProdExtension(left, right) => is_empty(left) && is_empty(right)
    | Prod(items)
    | TypTuple(items) => List.for_all(is_empty, items)
    | Sum(variants) =>
      List.for_all(
        fun
        | ConstructorMap.BadEntry(inner) => is_empty(inner)
        | ConstructorMap.Variant(_, _, _) => false,
        variants,
      )
    | Unknown(_)
    | Atom(_)
    | DrvQuoteTy(_)
    | Var(_)
    | ExplicitNonlabel
    | Label(_)
    | ProofOf(_)
    | Sig(_) => false
    };
  };

let rec collect_constraints =
        (
          ~replace_bound=false,
          ctx,
          bound: list(string),
          schema: t,
          demand: t,
        ) =>
  switch (term_of(schema)) {
  | Var(name) when List.mem(name, bound) => (
      replace_bound ? demand : schema,
      [(name, demand)],
    )
  | _ =>
    let collect_pair = (schema, demand) =>
      collect_constraints(~replace_bound, ctx, bound, schema, demand);
    let collect_many = (schemas, demands) =>
      List.map2(collect_pair, schemas, demands);
    let collect_under = (pattern, schema, demand) => {
      let shadowed =
        TPat.binders_of(pattern) |> List.filter_map(TPat.tyvar_of_utpat);
      collect_constraints(
        ~replace_bound,
        ctx,
        List.filter(name => !List.mem(name, shadowed), bound),
        schema,
        demand,
      );
    };
    let fallback = () =>
      if (equal(schema, demand)) {
        (demand, []);
      } else {
        let schema' = weak_head_normalize(ctx, schema);
        let demand' = weak_head_normalize(ctx, demand);
        if (!equal(schema, schema') || !equal(demand, demand')) {
          collect_constraints(~replace_bound, ctx, bound, schema', demand');
        } else {
          (demand, []);
        };
      };
    switch (term_of(schema), term_of(demand)) {
    | (Parens(schema), _) => collect_pair(schema, demand)
    | (_, Parens(demand)) => collect_pair(schema, demand)
    | (Projector(_, schema), _) => collect_pair(schema, demand)
    | (_, Projector(_, demand)) => collect_pair(schema, demand)
    | (List(schema), List(demand)) =>
      let (matched, constraints) = collect_pair(schema, demand);
      (List(matched) |> temp, constraints);
    | (TypFun(pattern, schema), TypFun(_, demand)) =>
      let (matched, constraints) = collect_under(pattern, schema, demand);
      (TypFun(pattern, matched) |> temp, constraints);
    | (Rec(pattern, schema), Rec(_, demand)) =>
      let (matched, constraints) = collect_under(pattern, schema, demand);
      (Rec(pattern, matched) |> temp, constraints);
    | (Poly(pattern, schema), Poly(_, demand)) =>
      let (matched, constraints) = collect_under(pattern, schema, demand);
      (Poly(pattern, matched) |> temp, constraints);
    | (Arrow(s1, s2), Arrow(d1, d2)) =>
      let (m1, c1) = collect_pair(s1, d1);
      let (m2, c2) = collect_pair(s2, d2);
      (Arrow(m1, m2) |> temp, c1 @ c2);
    | (TupLabel(s1, s2), TupLabel(d1, d2)) =>
      let (m1, c1) = collect_pair(s1, d1);
      let (m2, c2) = collect_pair(s2, d2);
      (TupLabel(m1, m2) |> temp, c1 @ c2);
    | (TypParamAp(s1, s2), TypParamAp(d1, d2)) =>
      let (m1, c1) = collect_pair(s1, d1);
      let (m2, c2) = collect_pair(s2, d2);
      (TypParamAp(m1, m2) |> temp, c1 @ c2);
    | (ProdProjection(s1, s2), ProdProjection(d1, d2)) =>
      let (m1, c1) = collect_pair(s1, d1);
      let (m2, c2) = collect_pair(s2, d2);
      (ProdProjection(m1, m2) |> temp, c1 @ c2);
    | (ProdExtension(s1, s2), ProdExtension(d1, d2)) =>
      let (m1, c1) = collect_pair(s1, d1);
      let (m2, c2) = collect_pair(s2, d2);
      (ProdExtension(m1, m2) |> temp, c1 @ c2);
    | (Prod(schemas), Prod(demands))
    | (TypTuple(schemas), TypTuple(demands))
        when List.length(schemas) == List.length(demands) =>
      let matches = collect_many(schemas, demands);
      let matched = List.map(fst, matches);
      let constraints = List.concat_map(snd, matches);
      switch (term_of(schema)) {
      | Prod(_) => (Prod(matched) |> temp, constraints)
      | TypTuple(_) => (TypTuple(matched) |> temp, constraints)
      | _ => (demand, [])
      };
    | (Sum(schemas), Sum(demands)) =>
      let find_variant = name =>
        List.find_map(
          fun
          | ConstructorMap.Variant(other, _, payload)
              when Constructor.equal(name, other) =>
            Some(payload)
          | ConstructorMap.Variant(_, _, _)
          | ConstructorMap.BadEntry(_) => None,
          demands,
        );
      let matches =
        List.map(
          fun
          | ConstructorMap.Variant(name, annotation, Some(schema)) => {
              let target =
                switch (find_variant(name)) {
                | Some(Some(target)) => target
                | Some(None)
                | None => gap
                };
              let (matched, constraints) = collect_pair(schema, target);
              let variant =
                is_empty(target)
                  ? ConstructorMap.BadEntry(gap)
                  : ConstructorMap.Variant(name, annotation, Some(matched));
              (variant, constraints);
            }
          | ConstructorMap.Variant(name, annotation, None) => {
              let variant =
                switch (find_variant(name)) {
                | Some(None) =>
                  ConstructorMap.Variant(name, annotation, None)
                | Some(Some(_))
                | None => ConstructorMap.BadEntry(gap)
                };
              (variant, []);
            }
          | ConstructorMap.BadEntry(_) => (ConstructorMap.BadEntry(gap), []),
          schemas,
        );
      (Sum(List.map(fst, matches)) |> temp, List.concat_map(snd, matches));
    | _ => fallback()
    };
  };

// Co-Heyting subtraction
let rec subtract = (ctx: Ctx.t, left: t, right: t): t =>
  if (is_gap(left)) {
    gap;
  } else if (is_gap(right)) {
    left;
  } else if (equal(left, right)) {
    gap;
  } else {
    let subtract_pair = (left, right) => subtract(ctx, left, right);
    let fallback = () => {
      let left' = weak_head_normalize(ctx, left);
      let right' = weak_head_normalize(ctx, right);
      if (!equal(left, left') || !equal(right, right')) {
        subtract(ctx, left', right');
      } else {
        meet(ctx, left, right) == None ? left : gap;
      };
    };
    switch (term_of(left), term_of(right)) {
    | (Parens(inner), _) => subtract(ctx, inner, right)
    | (_, Parens(inner)) => subtract(ctx, left, inner)
    | (Projector(_, inner), _) => subtract(ctx, inner, right)
    | (_, Projector(_, inner)) => subtract(ctx, left, inner)
    | (List(left), List(right)) =>
      List(subtract(ctx, left, right)) |> temp
    | (TypFun(pattern, left), TypFun(_, right)) =>
      TypFun(pattern, subtract(ctx, left, right)) |> temp
    | (Rec(pattern, left), Rec(_, right)) =>
      Rec(pattern, subtract(ctx, left, right)) |> temp
    | (Poly(pattern, left), Poly(_, right)) =>
      Poly(pattern, subtract(ctx, left, right)) |> temp
    | (Arrow(l1, l2), Arrow(r1, r2)) =>
      Arrow(subtract_pair(l1, r1), subtract_pair(l2, r2)) |> temp
    | (TupLabel(l1, l2), TupLabel(r1, r2)) =>
      TupLabel(subtract_pair(l1, r1), subtract_pair(l2, r2)) |> temp
    | (TypParamAp(l1, l2), TypParamAp(r1, r2)) =>
      TypParamAp(subtract_pair(l1, r1), subtract_pair(l2, r2)) |> temp
    | (ProdProjection(l1, l2), ProdProjection(r1, r2)) =>
      ProdProjection(subtract_pair(l1, r1), subtract_pair(l2, r2)) |> temp
    | (ProdExtension(l1, l2), ProdExtension(r1, r2)) =>
      ProdExtension(subtract_pair(l1, r1), subtract_pair(l2, r2)) |> temp
    | (Prod(lefts), Prod(rights))
    | (TypTuple(lefts), TypTuple(rights))
        when List.length(lefts) == List.length(rights) =>
      let residual = List.map2(subtract_pair, lefts, rights);
      switch (term_of(left)) {
      | Prod(_) => Prod(residual) |> temp
      | TypTuple(_) => TypTuple(residual) |> temp
      | _ => left
      };
    | (Sum(left_variants), Sum(right_variants)) =>
      let find_variant = name =>
        List.find_map(
          fun
          | ConstructorMap.Variant(other, _, payload)
              when Constructor.equal(name, other) =>
            Some(payload)
          | ConstructorMap.Variant(_, _, _)
          | ConstructorMap.BadEntry(_) => None,
          right_variants,
        );
      Sum(
        List.map(
          fun
          | ConstructorMap.Variant(name, annotation, payload) as left =>
            switch (find_variant(name), payload) {
            | (None, _) => left
            | (Some(None), None) => ConstructorMap.BadEntry(gap)
            | (Some(None), Some(_)) => left
            | (Some(Some(_)), None) => ConstructorMap.BadEntry(gap)
            | (Some(Some(right)), Some(left)) =>
              let residual = subtract(ctx, left, right);
              is_empty(residual)
                ? ConstructorMap.BadEntry(gap)
                : ConstructorMap.Variant(name, annotation, Some(residual));
            }
          | ConstructorMap.BadEntry(left) => ConstructorMap.BadEntry(left),
          left_variants,
        ),
      )
      |> temp;
    | _ => fallback()
    };
  };

let overlap = (ctx: Ctx.t, query: t, supplied: t): t =>
  subtract(ctx, query, subtract(ctx, query, supplied));

let children = (ty: t): list(t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Var(_)
  | ExplicitNonlabel
  | Label(_)
  | ProofOf(_)
  | Sig(_) => []
  | List(inner)
  | TypFun(_, inner)
  | Parens(inner)
  | Projector(_, inner)
  | Rec(_, inner)
  | Poly(_, inner) => [inner]
  | Arrow(left, right)
  | TupLabel(left, right)
  | TypParamAp(left, right)
  | ProdProjection(left, right)
  | ProdExtension(left, right) => [left, right]
  | Prod(items)
  | TypTuple(items) => items
  | Sum(variants) =>
    List.concat_map(
      fun
      | ConstructorMap.Variant(name, ann, payload) => [
          IdTagged.mk_internal(ann.ids, Var(name): term),
          ...Option.to_list(payload),
        ]
      | ConstructorMap.BadEntry(inner) => [inner],
      variants,
    )
  };

let rebuild = (shape: t, replacements: list(t)): option(t) => {
  let (term, rewrap) = unwrap(shape);
  let one = f =>
    switch (replacements) {
    | [inner] => Some(f(inner) |> rewrap)
    | _ => None
    };
  let two = f =>
    switch (replacements) {
    | [left, right] => Some(f(left, right) |> rewrap)
    | _ => None
    };
  let many = (items, f) =>
    List.length(items) == List.length(replacements)
      ? Some(f(replacements) |> rewrap) : None;
  switch (term) {
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Var(_)
  | ExplicitNonlabel
  | Label(_)
  | ProofOf(_)
  | Sig(_) => replacements == [] ? Some(shape) : None
  | List(_) => one(inner => List(inner))
  | TypFun(binder, _) => one(inner => TypFun(binder, inner))
  | Parens(_) => one(inner => Parens(inner))
  | Projector(data, _) => one(inner => Projector(data, inner))
  | Rec(binder, _) => one(inner => Rec(binder, inner))
  | Poly(binder, _) => one(inner => Poly(binder, inner))
  | Arrow(_, _) => two((left, right) => Arrow(left, right))
  | TupLabel(_, _) => two((left, right) => TupLabel(left, right))
  | TypParamAp(_, _) => two((left, right) => TypParamAp(left, right))
  | ProdProjection(_, _) =>
    two((left, right) => ProdProjection(left, right))
  | ProdExtension(_, _) => two((left, right) => ProdExtension(left, right))
  | Prod(items) => many(items, items => Prod(items))
  | TypTuple(items) => many(items, items => TypTuple(items))
  | Sum(variants) =>
    let variant = (ann, named, payload) =>
      switch (term_of(named)) {
      | Var(name') => ConstructorMap.Variant(name', ann, payload)
      | _ =>
        ConstructorMap.BadEntry(
          switch (payload) {
          | Some(payload) => payload
          | None => named
          },
        )
      };
    let rec refill = (variants, replacements) =>
      switch (variants, replacements) {
      | ([], []) => Some([])
      | (
          [ConstructorMap.Variant(_, ann, None), ...rest],
          [named, ...replacements],
        ) =>
        refill(rest, replacements)
        |> Option.map(List.cons(variant(ann, named, None)))
      | (
          [ConstructorMap.Variant(_, ann, Some(_)), ...rest],
          [named, payload, ...replacements],
        ) =>
        refill(rest, replacements)
        |> Option.map(List.cons(variant(ann, named, Some(payload))))
      | ([ConstructorMap.BadEntry(_), ...rest], [inner, ...replacements]) =>
        refill(rest, replacements)
        |> Option.map(List.cons(ConstructorMap.BadEntry(inner)))
      | _ => None
      };
    refill(variants, replacements)
    |> Option.map(variants => Sum(variants) |> rewrap);
  };
};

let embed = (~build, shape: t, components: list(t), i: int, child: t): t => {
  let rec mask = (ty: t, known: t): t =>
    if (is_gap(known)) {
      gap;
    } else if (is_gap(ty)) {
      known;
    } else {
      switch (children(ty), children(known)) {
      | (kids, parts)
          when kids != [] && List.length(kids) == List.length(parts) =>
        rebuild(ty, List.map2(mask, kids, parts))
        |> Option.value(~default=ty)
      | _ => ty
      };
    };
  let replacements =
    List.mapi(
      (j, sibling) =>
        if (j == i) {
          mask(sibling, child);
        } else {
          switch (term_of(sibling)) {
          | Label(_)
          | ExplicitNonlabel => sibling
          | _ => gap
          };
        },
      components,
    );
  rebuild(shape, replacements) |> Option.value(~default=build(replacements));
};

let meet_gap = (ctx: Ctx.t, left: t, right: t): t =>
  if (is_gap(left)) {
    right;
  } else if (is_gap(right)) {
    left;
  } else {
    meet(ctx, left, right) |> Option.value(~default=left);
  };

let meet_gap_all = (ctx: Ctx.t, tys: list(t)): t =>
  List.fold_left(meet_gap(ctx), gap, tys);

let matched_query = (ctx: Ctx.t, query: t): t => {
  let rec peel = (binders, definition) =>
    switch (term_of(definition)) {
    | TypFun(pattern, body) =>
      peel(binders @ TPat.binders_of(pattern), body)
    | _ => (binders, definition)
    };
  switch (term_of(query)) {
  | TypParamAp({term: Var(name), _}, arguments) =>
    switch (Ctx.lookup_tvar(ctx, name)) {
    | Some(Singleton(definition)) =>
      let (binders, body) = peel([], definition);
      let arguments =
        switch (term_of(arguments)) {
        | TypTuple(arguments) => arguments
        | _ => [arguments]
        };
      let binders = List.filter_map(TPat.tyvar_of_utpat, binders);
      let bindings =
        List.length(binders) == List.length(arguments)
          ? List.combine(binders, arguments) : [];
      let rec go = ty =>
        switch (term_of(ty)) {
        | Var(name) =>
          List.find_map(
            ((bound, query)) => bound == name ? Some(query) : None,
            bindings,
          )
          |> Option.value(~default=gap)
        | TypParamAp(fn, arg) =>
          let arg = go(arg);
          is_empty(arg) ? gap : TypParamAp(fn, arg) |> temp;
        | Sum(_) => gap
        | _ =>
          switch (children(ty)) {
          | [] => gap
          | kids =>
            rebuild(ty, List.map(go, kids)) |> Option.value(~default=gap)
          }
        };
      bindings == [] ? query : go(body);
    | _ => query
    }
  | _ => query
  };
};

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
  | TypFun(_)
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
  | TypFun(_)
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
  | TypFun(_, t) => is_syn(t)
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
  | TypFun(_, _)
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
  | Parens(inner) => "(" ++ pretty_print_tvar(inner) ++ ")"
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
  | TypFun(tv, t) =>
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
