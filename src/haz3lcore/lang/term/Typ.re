open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | SynSwitch
  | Internal
  | Int
  | Float
  | Bool
  | String
  | Arrow
  | Prod
  | Sum
  | List
  | Var
  | Constructor
  | Parens
  | Ap
  | Rec
  | Forall;

include TermBase.Typ;

let term_of: t => term = IdTagged.term_of;
let unwrap: t => (term, term => t) = IdTagged.unwrap;
let fresh: term => t = IdTagged.fresh;
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let temp: term => t = term => {term, ids: [Id.invalid], copied: false};
let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)) =>
  switch (tms) {
  | [] => Unknown(Hole(EmptyHole))
  | [_, ..._] => Unknown(Hole(MultiHole(tms)))
  };

let cls_of_term: term => cls =
  fun
  | Unknown(Hole(Invalid(_))) => Invalid
  | Unknown(Hole(EmptyHole)) => EmptyHole
  | Unknown(Hole(MultiHole(_))) => MultiHole
  | Unknown(SynSwitch) => SynSwitch
  | Unknown(Internal) => Internal
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(_) => List
  | Arrow(_) => Arrow
  | Var(_) => Var
  | Prod(_) => Prod
  | Parens(_) => Parens
  | Ap(_) => Ap
  | Sum(_) => Sum
  | Rec(_) => Rec
  | Forall(_) => Forall;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type"
  | MultiHole => "Broken type"
  | EmptyHole => "Empty type hole"
  | SynSwitch => "Synthetic type"
  | Internal => "Internal type"
  | Int
  | Float
  | String
  | Bool => "Base type"
  | Var => "Type variable"
  | Constructor => "Sum constructor"
  | List => "List type"
  | Arrow => "Function type"
  | Prod => "Product type"
  | Sum => "Sum type"
  | Parens => "Parenthesized type"
  | Ap => "Constructor application"
  | Rec => "Recursive type"
  | Forall => "Forall type";

let rec is_arrow = (typ: t) => {
  switch (typ.term) {
  | Parens(typ) => is_arrow(typ)
  | Arrow(_) => true
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String
  | List(_)
  | Prod(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Forall(_)
  | Rec(_) => false
  };
};

let rec has_arrow = (ctx: Ctx.t, typ: t) =>
  switch (typ.term) {
  | Parens(typ) => has_arrow(ctx, typ)
  | Arrow(_)
  | Forall(_) => true
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => false
  | Var(tvar) =>
    switch (Ctx.lookup_alias(ctx, tvar)) {
    | Some(t) => has_arrow(ctx, t)
    | None => false
    }
  | List(t) => has_arrow(ctx, t)
  | Rec(tvar, tbody) => has_arrow(Ctx.extend_dummy_tvar(ctx, tvar), tbody)
  | Sum(sm) =>
    List.exists(
      fun
      | ConstructorMap.Variant(_, _, None) => false
      | Variant(_, _, Some(t)) => has_arrow(ctx, t)
      // TODO(zhiyao): Idk if this is correct
      | BadEntry(_) => false,
      sm,
    )
  // TODO(zhiyao): Idk if this is correct
  | Ap(t1, t2) => has_arrow(ctx, t1) || has_arrow(ctx, t2)
  | Prod(tys) => List.exists(has_arrow(ctx), tys)
  };

let rec is_forall = (typ: t) => {
  switch (typ.term) {
  | Parens(typ) => is_forall(typ)
  | Forall(_) => true
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String
  | Arrow(_)
  | List(_)
  | Prod(_)
  | Var(_)
  | Ap(_)
  | Sum(_)
  | Rec(_) => false
  };
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
    (p1: type_provenance, p2: type_provenance): type_provenance =>
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

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => []
  | Ap(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | Parens(ty) => free_vars(~bound, ty)
  | List(ty) => free_vars(~bound, ty)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) => ConstructorMap.free_variables(free_vars(~bound), sm)
  | Prod(tys) => ListUtil.flat_map(free_vars(~bound), tys)
  | Rec(x, ty)
  | Forall(x, ty) =>
    free_vars(~bound=(x |> TPat.tyvar_of_utpat |> Option.to_list) @ bound, ty)
  };

let var_count = ref(0);
let fresh_var = (var_name: string) => {
  let x = var_count^;
  var_count := x + 1;
  var_name ++ "_α" ++ string_of_int(x);
};

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec(tp, ty_body) => subst(ty, tp, ty_body)
  | _ => ty
  };

/* Type Equality: This coincides with alpha equivalence for normalized types.
   Other types may be equivalent but this will not detect so if they are not normalized. */
let eq = (t1: t, t2: t): bool => fast_equal(t1, t2);

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join = (~resolve=false, ~fix, ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
  let join' = join(~resolve, ~fix, ctx);
  switch (term_of(ty1), term_of(ty2)) {
  | (_, Parens(ty2)) => join'(ty1, ty2)
  | (Parens(ty1), _) => join'(ty1, ty2)
  | (_, Unknown(Hole(_))) when fix =>
    /* NOTE(andrew): This is load bearing
       for ensuring that function literals get appropriate
       casts. Documentation/Dynamics has regression tests */
    Some(ty2)
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(join_type_provenance(p1, p2)) |> temp)
  | (Unknown(_), _) => Some(ty2)
  | (_, Unknown(Internal | SynSwitch)) => Some(ty1)
  | (Var(n1), Var(n2)) =>
    if (n1 == n2) {
      Some(ty1);
    } else {
      let* ty1 = Ctx.lookup_alias(ctx, n1);
      let* ty2 = Ctx.lookup_alias(ctx, n2);
      let+ ty_join = join'(ty1, ty2);
      !resolve && eq(ty1, ty_join) ? ty1 : ty_join;
    }
  | (Var(name), _) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_join = join'(ty_name, ty2);
    !resolve && eq(ty_name, ty_join) ? ty1 : ty_join;
  | (_, Var(name)) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_join = join'(ty_name, ty1);
    !resolve && eq(ty_name, ty_join) ? ty2 : ty_join;
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(tp2)) {
      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
      | None => ty1
      };
    let+ ty_body = join(~resolve, ~fix, ctx, ty1', ty2);
    Rec(tp1, ty_body) |> temp;
  | (Rec(_), _) => None
  | (Forall(x1, ty1), Forall(x2, ty2)) =>
    let ctx = Ctx.extend_dummy_tvar(ctx, x1);
    let ty1' =
      switch (TPat.tyvar_of_utpat(x2)) {
      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
      | None => ty1
      };
    let+ ty_body = join(~resolve, ~fix, ctx, ty1', ty2);
    Forall(x1, ty_body) |> temp;
  /* Note for above: there is no danger of free variable capture as
     subst itself performs capture avoiding substitution. However this
     may generate internal type variable names that in corner cases can
     be exposed to the user. We preserve the variable name of the
     second type to preserve synthesized type variable names, which
     come from user annotations. */
  | (Forall(_), _) => None
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (String, String) => Some(ty1)
  | (String, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = join'(ty1, ty1');
    let+ ty2 = join'(ty2, ty2');
    Arrow(ty1, ty2) |> temp;
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    let* tys = ListUtil.map2_opt(join', tys1, tys2);
    let+ tys = OptUtil.sequence(tys);
    Prod(tys) |> temp;
  | (Prod(_), _) => None
  | (Sum(sm1), Sum(sm2)) =>
    let+ sm' = ConstructorMap.join(eq, join(~resolve, ~fix, ctx), sm1, sm2);
    Sum(sm') |> temp;
  | (Sum(_), _) => None
  | (List(ty1), List(ty2)) =>
    let+ ty = join'(ty1, ty2);
    List(ty) |> temp;
  | (List(_), _) => None
  | (Ap(_), _) => failwith("Type join of ap")
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
  | (Int, _)
  | (Float, _)
  | (Bool, _)
  | (String, _)
  | (Var(_), _)
  | (Ap(_), _)
  | (Rec(_), _)
  | (Forall(_), _) => t1
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
  | (Sum(sm1), Sum(sm2)) =>
    let sm' = ConstructorMap.match_synswitch(match_synswitch, eq, sm1, sm2);
    Sum(sm') |> rewrap1;
  | (Sum(_), _) => t1
  };
};

let join_fix = join(~fix=true);

let join_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => OptUtil.and_then(join(~fix=false, ctx, ty), acc),
    Some(empty),
    ts,
  );

let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
  join(~fix=false, ctx, ty1, ty2) != None;

let rec weak_head_normalize = (ctx: Ctx.t, ty: t): t =>
  switch (term_of(ty)) {
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => weak_head_normalize(ctx, ty)
    | None => ty
    }
  | _ => ty
  };

let rec normalize = (ctx: Ctx.t, ty: t): t => {
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => ty
  | Parens(t) => Parens(normalize(ctx, t)) |> rewrap
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | Ap(t1, t2) => Ap(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(normalize(ctx), ts)) |> rewrap
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

let rec matched_forall_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_forall_strict(ctx, ty)
  | Forall(t, ty) => Some((Some(t), ty))
  | Unknown(SynSwitch) => Some((None, Unknown(SynSwitch) |> temp))
  | _ => None // (None, Unknown(Internal) |> temp)
  };

let matched_forall = (ctx, ty) =>
  matched_forall_strict(ctx, ty)
  |> Option.value(~default=(None, Unknown(Internal) |> temp));

let rec matched_prod_strict = (ctx, length, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => matched_prod_strict(ctx, length, ty)
  | Prod(tys) when List.length(tys) == length => Some(tys)
  | Unknown(SynSwitch) =>
    Some(List.init(length, _ => Unknown(SynSwitch) |> temp))
  | _ => None
  };

let matched_prod = (ctx, length, ty) =>
  matched_prod_strict(ctx, length, ty)
  |> Option.value(~default=List.init(length, _ => Unknown(Internal) |> temp));

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

let rec matched_args = (ctx, default_arity, ty) => {
  let ty' = weak_head_normalize(ctx, ty);
  switch (term_of(ty')) {
  | Parens(ty) => matched_args(ctx, default_arity, ty)
  | Prod([_, ..._] as tys) => tys
  | Unknown(_) => List.init(default_arity, _ => ty')
  | _ => [ty']
  };
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
      | Rec({term: Var(x), _}, ty_body) =>
        switch (Ctx.lookup_alias(ctx, x)) {
        | None => unroll(ty)
        | Some(_) => ty_body
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

let rec is_unknown = (ty: t): bool =>
  switch (ty |> term_of) {
  | Parens(x) => is_unknown(x)
  | Unknown(_) => true
  | _ => false
  };

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (ty: t): bool =>
  switch (term_of(ty)) {
  | Parens(ty) => needs_parens(ty)
  | Ap(_)
  | Unknown(_)
  | Int
  | Float
  | String
  | Bool
  | Var(_) => false
  | Rec(_, _)
  | Forall(_, _) => true
  | List(_) => false /* is already wrapped in [] */
  | Arrow(_, _) => true
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

/* Essentially recreates haz3lweb/view/Type.re's view_ty but with string output */
let rec pretty_print = (ty: t): string =>
  switch (term_of(ty)) {
  | Parens(ty) => pretty_print(ty)
  | Ap(_)
  | Unknown(_) => "?"
  | Int => "Int"
  | Float => "Float"
  | Bool => "Bool"
  | String => "String"
  | Var(tvar) => tvar
  | List(t) => "[" ++ pretty_print(t) ++ "]"
  | Arrow(t1, t2) => paren_pretty_print(t1) ++ "->" ++ pretty_print(t2)
  | Sum(sm) =>
    switch (sm) {
    | [] => "+?"
    | [t0] => "+" ++ ctr_pretty_print(t0)
    | [t0, ...ts] =>
      List.fold_left(
        (acc, t) => acc ++ "+" ++ ctr_pretty_print(t),
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
  | Rec(tv, t) => "rec " ++ pretty_print_tvar(tv) ++ "->" ++ pretty_print(t)
  | Forall(tv, t) =>
    "forall " ++ pretty_print_tvar(tv) ++ "->" ++ pretty_print(t)
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
