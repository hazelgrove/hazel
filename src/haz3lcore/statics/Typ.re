include TypBase.Typ;
module Ctx = TypBase.Ctx;
open Util;
open OptUtil.Syntax;

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

let matched_arrow_mode: mode => (mode, mode) =
  fun
  | SynFun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let matched_prod_mode = (mode: mode, length): list(mode) =>
  switch (mode) {
  | Ana(Prod(ana_tys)) when List.length(ana_tys) == length =>
    List.map(ty => Ana(ty), ana_tys)
  | Ana(Unknown(prod)) => List.init(length, _ => Ana(Unknown(prod)))
  | _ => List.init(length, _ => Syn)
  };

let matched_list: t => t =
  fun
  | List(ty) => ty
  | Unknown(prov) => Unknown(prov)
  | _ => Unknown(Internal);

let matched_list_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => Ana(matched_list(ty));

let ap_mode: mode = SynFun;

let is_rec: t => bool =
  fun
  | Rec(_, _) => true
  | _ => false;

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
  | (Rec(_), _) => eq(unroll(t1), t2)
  | (_, Rec(_)) => eq(t1, unroll(t2))
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
  | (Rec(x1, ty1), Rec(x2, ty2)) =>
    let+ ty_body = join(ctx, ty1, subst(Var(x1), x2, ty2));
    Rec(x1, ty_body);
  | (Rec(_), _) =>
    let+ _ = join'(unroll(ty1), ty2);
    ty1;
  | (_, Rec(_)) =>
    let+ _ = join'(ty1, unroll(ty2));
    ty2;
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

let sum_entry = (t: Token.t, tags: sum_map): option(sum_entry) =>
  List.find_map(
    fun
    | (tag, typ) when tag == t => Some((tag, typ))
    | _ => None,
    tags,
  );

let ana_sum = (tag: Token.t, sm: sum_map, ty_ana: t): option(t) =>
  /* Returns the type of a tag if that tag is given a type by the sum
     type ty_ana having tags as variants. If tag is a nullart constructor,
     ty_ana itself is returned; otherwise an arrow from tag's parameter
     type to ty_ana */
  switch (sum_entry(tag, sm)) {
  | Some((_, Some(ty_in))) =>
    print_endline("ana_sum: Some(ty_in)");
    ty_in |> show |> print_endline;
    Some(Arrow(ty_in, ty_ana));
  | Some((_, None)) =>
    print_endline("ana_sum: None");
    Some(ty_ana);
  | None => None
  };

let tag_ana_typ = (ctx: Ctx.t, mode: mode, tag: Token.t): option(t) =>
  /* If a tag is being analyzed against (an arrow type returning)
     a sum type having that tag as a variant, we consider the
     tag's type to be determined by the sum type */
  switch (mode) {
  | Ana(Arrow(_, ty_ana))
  | Ana(ty_ana) =>
    switch (normalize_shallow(ctx, ty_ana)) {
    | Sum(sm) =>
      ana_sum(tag, sm, ty_ana);
    | Rec(_, Sum(_sm)) =>
      // note: unroll here is JUST to get right tag types, NOT typ itself
      // this is import as if the type name P is not in scope
      // we don't want a rec constructor to have type P -> something
      switch (unroll(ty_ana)) {
      | Sum(sm) =>
        ana_sum(tag, sm, ty_ana);
      | _ => None
      };
    | _ => None
    }
  | _ => None
  };

let tag_ap_mode = (ctx: Ctx.t, mode: mode, name: Token.t): mode =>
  /* If a tag application is being analyzed against a sum type for
     which that tag is a variant, then we consider the tag to be in
     analytic mode against an arrow returning that sum type; otherwise
     we use the typical mode for function applications */
  switch (tag_ana_typ(ctx, mode, name)) {
  | Some(Arrow(_) as ty_ana) => Ana(ty_ana)
  | Some(ty_ana) => Ana(Arrow(Unknown(Internal), ty_ana))
  | _ => ap_mode
  };
