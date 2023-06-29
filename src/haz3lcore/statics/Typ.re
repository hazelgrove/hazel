include TypBase.Typ;
module Ctx = TypBase.Ctx;
open Util;
open OptUtil.Syntax;

/* Strip location information from a list of sources */
let of_source = List.map((source: source) => source.ty);

/* How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let join_type_provenance =
    (p1: type_provenance, p2: type_provenance): type_provenance =>
  switch (p1, p2) {
  | (Free(tv1), Free(tv2)) when TypVar.eq(tv1, tv2) => Free(tv1)
  | (Internal | Free(_), _)
  | (_, Internal | Free(_)) => Internal
  | (TypeHole, TypeHole | SynSwitch)
  | (SynSwitch, TypeHole) => TypeHole
  | (SynSwitch, SynSwitch) => SynSwitch
  };

let matched_arrow: t => (t, t) =
  fun
  | Arrow(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(SynSwitch) => (Unknown(SynSwitch), Unknown(SynSwitch))
  | _ => (Unknown(Internal), Unknown(Internal));

let matched_prod: (int, t) => list(t) =
  length =>
    fun
    | Prod(tys) when List.length(tys) == length => tys
    | Unknown(SynSwitch) => List.init(length, _ => Unknown(SynSwitch))
    | _ => List.init(length, _ => Unknown(Internal));

let matched_list: t => t =
  fun
  | List(ty) => ty
  | Unknown(SynSwitch) => Unknown(SynSwitch)
  | _ => Unknown(Internal);

let precedence_Prod = 1;
let precedence_Arrow = 2;
let precedence_Sum = 3;
let precedence_const = 4;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | String
  | Unknown(_)
  | Var(_)
  | Rec(_)
  | Sum(_)
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Arrow(_, _) => precedence_Arrow
  };

let rec subst = (s: t, x: TypVar.t, ty: t) => {
  switch (ty) {
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Unknown(prov) => Unknown(prov)
  | Arrow(ty1, ty2) => Arrow(subst(s, x, ty1), subst(s, x, ty2))
  | Prod(tys) => Prod(List.map(subst(s, x), tys))
  | Sum(sm) => Sum(TagMap.map(Option.map(subst(s, x)), sm))
  | Rec(y, ty) when TypVar.eq(x, y) => Rec(y, ty)
  | Rec(y, ty) => Rec(y, subst(s, x, ty))
  | List(ty) => List(subst(s, x, ty))
  | Var(y) => TypVar.eq(x, y) ? s : Var(y)
  };
};

let unroll = (ty: t): t =>
  switch (ty) {
  | Rec(x, ty_body) => subst(ty, x, ty_body)
  | _ => ty
  };

/* Type Equality: At the moment, this coincides with alpha equivalence,
   but this will change when polymorphic types are implemented */
let rec eq = (t1: t, t2: t): bool => {
  switch (t1, t2) {
  | (Rec(x1, t1), Rec(x2, t2)) => eq(t1, subst(Var(x1), x2, t2))
  | (Rec(_), _) => false
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (String, String) => true
  | (String, _) => false
  | (Unknown(_), Unknown(_)) => true
  | (Unknown(_), _) => false
  | (Arrow(t1, t2), Arrow(t1', t2')) => eq(t1, t1') && eq(t2, t2')
  | (Arrow(_), _) => false
  | (Prod(tys1), Prod(tys2)) => List.equal(eq, tys1, tys2)
  | (Prod(_), _) => false
  | (List(t1), List(t2)) => eq(t1, t2)
  | (List(_), _) => false
  | (Sum(sm1), Sum(sm2)) => TagMap.equal(Option.equal(eq), sm1, sm2)
  | (Sum(_), _) => false
  | (Var(n1), Var(n2)) => n1 == n2
  | (Var(_), _) => false
  };
};

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (ty) {
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => []
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | List(ty) => free_vars(~bound, ty)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) =>
    ListUtil.flat_map(
      fun
      | None => []
      | Some(typ) => free_vars(~bound, typ),
      List.map(snd, sm),
    )
  | Prod(tys) => ListUtil.flat_map(free_vars(~bound), tys)
  | Rec(x, ty) => free_vars(~bound=[x, ...bound], ty)
  };

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join = (~resolve=false, ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
  let join' = join(~resolve, ctx);
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  | (Var(n1), Var(n2)) =>
    if (n1 == n2) {
      Some(Var(n1));
    } else {
      let* ty1 = Ctx.lookup_alias(ctx, n1);
      let* ty2 = Ctx.lookup_alias(ctx, n2);
      let+ ty_join = join'(ty1, ty2);
      !resolve && eq(ty1, ty_join) ? Var(n1) : ty_join;
    }
  | (Var(name), ty)
  | (ty, Var(name)) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_join = join'(ty_name, ty);
    !resolve && eq(ty_name, ty_join) ? Var(name) : ty_join;
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (Rec(x1, ty1), Rec(x2, ty2)) =>
    /* TODO:
         This code isn't fully correct, as we may be doing
         substitution on open terms; if x1 occurs in ty2,
         we should be substituting x1 for a fresh variable
         in ty2. This is annoying, and should be obviated
         by the forthcoming debruijn index implementation
       */
    let ctx = Ctx.extend_dummy_tvar(ctx, x1);
    let+ ty_body = join(ctx, ty1, subst(Var(x1), x2, ty2));
    Rec(x1, ty_body);
  | (Rec(_), _) => None
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (String, String) => Some(String)
  | (String, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = join'(ty1, ty1');
    let+ ty2 = join'(ty2, ty2');
    Arrow(ty1, ty2);
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    let* tys = ListUtil.map2_opt(join(ctx), tys1, tys2);
    let+ tys = OptUtil.sequence(tys);
    Prod(tys);
  | (Prod(_), _) => None
  | (Sum(sm1), Sum(sm2)) =>
    let (sorted1, sorted2) =
      /* If same order, retain order for UI */
      TagMap.same_tags_same_order(sm1, sm2)
        ? (sm1, sm2) : (TagMap.sort(sm1), TagMap.sort(sm2));
    let* ty = ListUtil.map2_opt(join_sum_entries(ctx), sorted1, sorted2);
    let+ ty = OptUtil.sequence(ty);
    Sum(ty);
  | (Sum(_), _) => None
  | (List(ty1), List(ty2)) =>
    let+ ty = join'(ty1, ty2);
    List(ty);
  | (List(_), _) => None
  };
}
and join_sum_entries =
    (ctx: Ctx.t, (tag1, ty1): sum_entry, (tag2, ty2): sum_entry)
    : option(sum_entry) =>
  switch (ty1, ty2) {
  | (None, None) when tag1 == tag2 => Some((tag1, None))
  | (Some(ty1), Some(ty2)) when tag1 == tag2 =>
    let+ ty_join = join(ctx, ty1, ty2);
    (tag1, Some(ty_join));
  | _ => None
  };

let join_all = (ctx: Ctx.t, ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => OptUtil.and_then(join(ctx, ty), acc),
    Some(Unknown(Internal)),
    ts,
  );

let rec weak_head_normalize = (ctx: Ctx.t, ty: t): t =>
  switch (ty) {
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => weak_head_normalize(ctx, ty)
    | None => ty
    }
  | _ => ty
  };

let rec normalize = (ctx: Ctx.t, ty: t): t => {
  switch (ty) {
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
  | List(t) => List(normalize(ctx, t))
  | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
  | Prod(ts) => Prod(List.map(normalize(ctx), ts))
  | Sum(ts) => Sum(TagMap.map(Option.map(normalize(ctx)), ts))
  | Rec(name, ty) =>
    /* NOTE: Dummy tvar added has fake id but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty))
  };
};

let sum_entry = (tag: Tag.t, tags: sum_map): option(sum_entry) =>
  List.find_map(
    fun
    | (t, typ) when Tag.equal(t, tag) => Some((t, typ))
    | _ => None,
    tags,
  );

let get_sum_tags = (ctx: Ctx.t, ty: t): option(sum_map) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (ty) {
  | Sum(sm) => Some(sm)
  | Rec(_) =>
    /* Note: We must unroll here to get right tag types;
       otherwise the rec parameter will leak */
    switch (unroll(ty)) {
    | Sum(sm) => Some(sm)
    | _ => None
    }
  | _ => None
  };
};
