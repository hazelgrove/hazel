include TypBase;
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

let matched_forall: t => ann(t) =
  fun
  | Forall(ann) => ann
  | Unknown(prov) => {item: Unknown(prov), name: "expected_forall"}
  | _ => {item: Unknown(Internal), name: "expected_forall"};

let matched_rec: t => ann(t) =
  fun
  | Rec(ann) => ann
  | Unknown(prov) => {item: Unknown(prov), name: "expected_rec"}
  | _ => {item: Unknown(Internal), name: "expected_rec"};

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

let matched_forall_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => {
      let ann = matched_forall(ty);
      Ana(ann.item);
    };

let matched_rec_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => {
      let ann = matched_rec(ty);
      Ana(ann.item);
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
  | Rec(_) => true
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
  | Forall(_)
  | Rec(_)
  | Sum(_)
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Arrow(_, _) => precedence_Arrow
  };

// Substitute the type variable with de bruijn index 0
let rec subst = (s: t, ~x: int=0, ty: t) => {
  let subst_keep = subst(~x, s);
  let subst_incr = subst(~x=x + 1, s);
  switch (ty) {
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Unknown(prov) => Unknown(prov)
  | Arrow(ty1, ty2) => Arrow(subst_keep(ty1), subst_keep(ty2))
  | Prod(tys) => Prod(List.map(ty => subst_keep(ty), tys))
  | Sum(sm) => Sum(TagMap.map(Option.map(subst_keep), sm))
  | List(ty) => List(subst_keep(ty))
  | Rec({item, name}) => Rec({item: subst_incr(item), name})
  | Forall({item, name}) => Forall({item: subst_incr(item), name})
  | Var({item: y, _}) => Some(x) == y ? s : ty
  };
};

// Lookup the type variable with de bruijn index 0
let rec lookup_surface = (~x: int=0, ty: t) => {
  let lookup_keep = lookup_surface(~x);
  let lookup_incr = lookup_surface(~x=x + 1);
  switch (ty) {
  | Int
  | Float
  | Bool
  | String
  | Unknown(_) => false
  | Arrow(ty1, ty2) => lookup_keep(ty1) || lookup_keep(ty2)
  | Prod(tys) => List.exists(lookup_keep, tys)
  | Sum(sm) =>
    TagMap.exists(
      x => Option.value(Option.map(lookup_keep, x), ~default=false),
      sm,
    )
  | List(ty) => lookup_keep(ty)
  | Rec({item, _}) => lookup_incr(item)
  | Forall({item, _}) => lookup_incr(item)
  | Var({item: y, _}) => Some(x) == y
  };
};

let unroll = (ty: t): t =>
  switch (ty) {
  | Rec({item: ty, _}) => subst(ty, ty)
  | _ => ty
  };

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join = (ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  | (Rec({item: t1, name}), Rec({item: t2, _})) =>
    switch (join(Ctx.add_abstract(ctx, name, -1), t1, t2)) {
    | Some(t) => Some(Rec({item: t, name}))
    | None => None
    }
  | (Rec(_), _) => None
  | (Forall({item: t1, name}), Forall({item: t2, _})) =>
    switch (join(Ctx.add_abstract(ctx, name, -1), t1, t2)) {
    | Some(t) => Some(Forall({item: t, name}))
    | None => None
    }
  | (Forall(_), _) => None
  | (Var({item: Some(n1), name}), Var({item: Some(n2), _})) =>
    let ob1 = Ctx.resolve_var_kind(ctx, ~remaining=n1);
    let ob2 = Ctx.resolve_var_kind(ctx, ~remaining=n2);
    Kind.Observation.eq(ob1, ob2)
      ? Some(Var({item: Some(n1), name})) : None;
  // switch (
  //   Ctx.lookup_typ_by_idx(ctx, ~i=n1),
  //   Ctx.lookup_typ_by_idx(ctx, ~i=n2),
  // ) {
  // | (Some(t1), Some(t2)) when eq(t1, t2) =>
  //   Some(Var({item: Some(n1), name}))
  // | _ => None
  // }
  // n1 == n2
  //   ? {
  //     Some(Var({item: Some(n1), name}));
  //   }
  //   : {
  //     Ctx.lookup_typ_by_idx(ctx, ~i=n1)
  //     == Ctx.lookup_typ_by_idx(ctx, ~i=n2)
  //       ? Some(Var({item: Some(n1), name})) : None;
  //   }
  | (Var(_), _) => None
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (String, String) => Some(String)
  | (String, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join(ctx, ty1_1, ty2_1), join(ctx, ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (List.map2(join(ctx), tys1, tys2) |> Util.OptUtil.sequence) {
      | None => None
      | Some(tys) => Some(Prod(tys))
      };
    }
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
  | (List(ty_1), List(ty_2)) =>
    switch (join(ctx, ty_1, ty_2)) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
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
    (acc, ty) => Util.OptUtil.and_then(join(ctx, ty), acc),
    Some(Unknown(Internal)),
    ts,
  );

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
  | Some((_, Some(ty_in))) => Some(Arrow(ty_in, ty_ana))
  | Some((_, None)) => Some(ty_ana)
  | None => None
  };

let tag_ana_typ = (mode: mode, tag: Token.t): option(t) =>
  /* If a tag is being analyzed against (an arrow type returning)
     a sum type having that tag as a variant, we consider the
     tag's type to be determined by the sum type */
  switch (mode) {
  | Ana(Arrow(_, ty_ana))
  | Ana(ty_ana) =>
    switch (ty_ana) {
    // switch (normalize_shallow(ctx, ty_ana)) {
    | Sum(sm)
    | Rec({item: Sum(sm), _}) => ana_sum(tag, sm, unroll(ty_ana))
    | _ => None
    }
  | _ => None
  };

let tag_ap_mode = (mode: mode, tag: Token.t): mode =>
  /* If a tag application is being analyzed against a sum type for
     which that tag is a variant, then we consider the tag to be in
     analytic mode against an arrow returning that sum type; otherwise
     we use the typical mode for function applications */
  switch (tag_ana_typ(mode, tag)) {
  | Some(Arrow(_) as ty_ana) => Ana(ty_ana)
  | Some(ty_ana) => Ana(Arrow(Unknown(Internal), ty_ana))
  | _ => ap_mode
  };

let rec normalize_shallow = (ctx: Ctx.t, ty: t): t =>
  switch (ty) {
  | Var({item: Some(idx), _}) =>
    switch (Ctx.lookup_typ_by_idx(ctx, ~i=idx)) {
    | Some(ty) => normalize_shallow(ctx, ty)
    | None => ty
    }
  | _ => ty
  };

let rec normalize = (ctx: Ctx.t, ty: t): t => {
  switch (ty) {
  | Var({item: Some(idx), _}) =>
    switch (Ctx.lookup_typ_by_idx(ctx, ~i=idx)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Var(_)
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => ty
  | List(t) => List(normalize(ctx, t))
  | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
  | Prod(ts) => Prod(List.map(normalize(ctx), ts))
  | Sum(ts) => Sum(Util.TagMap.map(Option.map(normalize(ctx)), ts))
  | Forall({item: ty, name}) =>
    /* NOTE: Fake -1 id below is a hack, but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Forall({item: normalize(Ctx.add_abstract(ctx, name, -1), ty), name})
  | Rec({item: ty, name}) =>
    /* NOTE: Fake -1 id below is a hack, but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec({item: normalize(Ctx.add_abstract(ctx, name, -1), ty), name})
  };
};
