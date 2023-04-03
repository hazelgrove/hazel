include TypBase.Typ;
module Ctx = TypBase.Ctx;
open Util;
open OptUtil.Syntax;

let of_mode = (mode: mode): t =>
  switch (mode) {
  | Syn => Unknown(SynSwitch)
  | SynFun => Arrow(Unknown(SynSwitch), Unknown(SynSwitch))
  | Ana(ty) => ty
  };

let rec to_string = (~holes=_ => "?", t: t): string => {
  let s = to_string(~holes);
  switch (t) {
  | Int => "Int"
  | Float => "Float"
  | Bool => "Bool"
  | String => "String"
  | Unknown(prov) => holes(prov)
  | Arrow(t1, t2) => "(" ++ s(t1) ++ " -> " ++ s(t2) ++ ")"
  | Prod(tys) => "(" ++ String.concat(", ", List.map(s, tys)) ++ ")"
  | Sum(sm) =>
    let entry = ((tag, ty)) =>
      switch (ty) {
      | None => tag
      | Some(t) => tag ++ "(" ++ s(t) ++ ")"
      };
    "(" ++ String.concat(" + ", List.map(entry, sm)) ++ ")";
  | Rec(x, ty) => "rec " ++ x ++ ".{" ++ s(ty) ++ "}"
  | List(ty) => "[" ++ s(ty) ++ "]"
  | Var(x) => x
  };
};

/* How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let join_type_provenance =
    (p1: type_provenance, p2: type_provenance): type_provenance =>
  switch (p1, p2) {
  | (TypeHole, TypeHole | Internal | SynSwitch)
  | (Internal | SynSwitch, TypeHole) => TypeHole
  | (Internal, Internal | SynSwitch)
  | (SynSwitch, Internal) => Internal
  | (SynSwitch, SynSwitch) => SynSwitch
  };

/* MATCHED JUDGEMENTS: Note that matched judgements work
   a bit different than hazel2 here since hole fixing is
   implicit. Somebody should check that what I'm doing
   here actually makes sense -Andrew */

let matched_arrow: t => (t, t) =
  fun
  | Arrow(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(prov) => (Unknown(prov), Unknown(prov))
  | _ => (Unknown(Internal), Unknown(Internal));

let matched_list: t => t =
  fun
  | List(ty) => ty
  | Unknown(prov) => Unknown(prov)
  | _ => Unknown(Internal);

let matched_arrow_mode: mode => (mode, mode) =
  fun
  | SynFun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let matched_list_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => Ana(matched_list(ty));

let matched_prod_modes = (mode: mode, length): list(mode) =>
  switch (mode) {
  | Ana(Prod(ana_tys)) when List.length(ana_tys) == length =>
    List.map(ty => Ana(ty), ana_tys)
  | Ana(Unknown(prod)) => List.init(length, _ => Ana(Unknown(prod)))
  | _ => List.init(length, _ => Syn)
  };

let matched_list_lit_modes = (mode: mode, length): list(mode) =>
  List.init(length, _ => matched_list_mode(mode));

/* Legacy precedence code from HTyp */
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

let rec subst = (s: t, x: Token.t, ty: t) => {
  switch (ty) {
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Unknown(prov) => Unknown(prov)
  | Arrow(ty1, ty2) => Arrow(subst(s, x, ty1), subst(s, x, ty2))
  | Prod(tys) => Prod(List.map(subst(s, x), tys))
  | Sum(sm) => Sum(TagMap.map(Option.map(subst(s, x)), sm))
  | Rec(y, ty) when Token.compare(x, y) == 0 => Rec(y, ty)
  | Rec(y, ty) => Rec(y, subst(s, x, ty))
  | List(ty) => List(subst(s, x, ty))
  | Var(y) => Token.compare(x, y) == 0 ? s : Var(y)
  };
};

let unroll = (ty: t): t =>
  switch (ty) {
  | Rec(x, ty_body) => subst(ty, x, ty_body)
  | _ => ty
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */
let rec eq = (t1: t, t2: t): bool => {
  let eq' = eq;
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
  | (Arrow(t1, t2), Arrow(t1', t2')) => eq'(t1, t1') && eq'(t2, t2')
  | (Arrow(_), _) => false
  | (Prod(tys1), Prod(tys2)) => List.equal(eq', tys1, tys2)
  | (Prod(_), _) => false
  | (List(t1), List(t2)) => eq'(t1, t2)
  | (List(_), _) => false
  | (Sum(sm1), Sum(sm2)) => TagMap.equal(Option.equal((==)), sm1, sm2)
  | (Sum(_), _) => false
  | (Var(n1), Var(n2)) => n1 == n2
  | (Var(_), _) => false
  };
};

let rec free_vars = (~bound=[], ty: t): list(Token.t) =>
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
  | Rec(x, ty) => free_vars(~bound=[x] @ bound, ty)
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
      resolve ? ty_join : Var(n1);
    }
  | (Var(name), ty)
  | (ty, Var(name)) =>
    let* ty_name = Ctx.lookup_alias(ctx, name);
    let+ ty_join = join'(ty_name, ty);
    resolve ? ty_join : Var(name);
  /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
  | (Rec(x1, ty1), Rec(x2, ty2)) =>
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
    let* ty =
      ListUtil.map2_opt(
        join_sum_entries(ctx),
        TagMap.sort(sm1),
        TagMap.sort(sm2),
      );
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

let rec normalize_shallow = (ctx: Ctx.t, ty: t): t =>
  switch (ty) {
  | Var(x) =>
    switch (Ctx.lookup_alias(ctx, x)) {
    | Some(ty) => normalize_shallow(ctx, ty)
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
  | Sum(ts) => Sum(Util.TagMap.map(Option.map(normalize(ctx)), ts))
  | Rec(x, ty) =>
    /* NOTE: Fake -1 id below is a hack, but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(x, normalize(Ctx.add_abstract(ctx, x, -1), ty))
  };
};

let sum_entry = (tag: Token.t, tags: sum_map): option(sum_entry) =>
  List.find_map(
    fun
    | (t, typ) when t == tag => Some((t, typ))
    | _ => None,
    tags,
  );

let get_sum_tags = (ctx: Ctx.t, ty: t): option(sum_map) => {
  let ty = normalize_shallow(ctx, ty);
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

let tag_ana_typ = (ctx: Ctx.t, mode: mode, tag: Token.t): option(t) => {
  /* If a tag is being analyzed against (an arrow type returning)
     a sum type having that tag as a variant, we consider the
     tag's type to be determined by the sum type */
  switch (mode) {
  | Ana(Arrow(_, ty_ana))
  | Ana(ty_ana) =>
    let* tags = get_sum_tags(ctx, ty_ana);
    let+ (_, ty_entry) = sum_entry(tag, tags);
    switch (ty_entry) {
    | None => ty_ana
    | Some(ty_in) => Arrow(ty_in, ty_ana)
    };
  | _ => None
  };
};

let ap_mode = (ctx, mode, tag_name: option(Token.t)): mode =>
  /* If a tag application is being analyzed against a sum type for
     which that tag is a variant, then we consider the tag to be in
     analytic mode against an arrow returning that sum type; otherwise
     we use the typical mode for function applications */
  switch (tag_name) {
  | Some(name) =>
    switch (tag_ana_typ(ctx, mode, name)) {
    | Some(Arrow(_) as ty_ana) => Ana(ty_ana)
    | Some(ty_ana) => Ana(Arrow(Unknown(Internal), ty_ana))
    | _ => SynFun
    }
  | None => SynFun
  };
