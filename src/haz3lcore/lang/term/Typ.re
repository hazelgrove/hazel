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
  | Rec;

include TermBase.Typ;

let term_of: t => term = IdTagged.term_of;
let unwrap: t => (term, term => t) = IdTagged.unwrap;
let fresh: term => t = IdTagged.fresh;
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
  | Rec(_) => Rec;

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
  | Rec => "Recursive Type";

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
  | Rec(_) => false
  };
};

/* Converts a syntactic type into a semantic type */
let rec to_typ: (Ctx.t, t) => t =
  (ctx, utyp) => {
    let (term, rewrap) = IdTagged.unwrap(utyp);
    switch (term) {
    | Unknown(_)
    | Bool
    | Int
    | Float
    | String => utyp
    | Var(name) =>
      switch (Ctx.lookup_tvar(ctx, name)) {
      | Some(_) => Var(name) |> rewrap
      | None => Unknown(Hole(Invalid(name))) |> rewrap
      }
    | Arrow(u1, u2) => Arrow(to_typ(ctx, u1), to_typ(ctx, u2)) |> rewrap
    | Prod(us) => Prod(List.map(to_typ(ctx), us)) |> rewrap
    | Sum(uts) => Sum(to_ctr_map(ctx, uts)) |> rewrap
    | List(u) => List(to_typ(ctx, u)) |> rewrap
    | Parens(u) => to_typ(ctx, u)
    /* The below cases should occur only inside sums */
    | Ap(_) => Unknown(Internal) |> rewrap
    | Rec({term: Invalid(_), _} as tpat, tbody)
    | Rec({term: EmptyHole, _} as tpat, tbody)
    | Rec({term: MultiHole(_), _} as tpat, tbody) =>
      Rec(tpat, to_typ(ctx, tbody)) |> rewrap
    | Rec({term: Var(name), _} as utpat, tbody) =>
      let ctx =
        Ctx.extend_tvar(
          ctx,
          {name, id: IdTagged.rep_id(utpat), kind: Abstract},
        );
      Rec(utpat, to_typ(ctx, tbody)) |> rewrap;
    };
  }
and to_variant:
  (Ctx.t, ConstructorMap.variant(t)) => ConstructorMap.variant(t) =
  ctx =>
    fun
    | Variant(ctr, ids, u) =>
      ConstructorMap.Variant(ctr, ids, Option.map(to_typ(ctx), u))
    | BadEntry(u) => ConstructorMap.BadEntry(to_typ(ctx, u))
and to_ctr_map = (ctx: Ctx.t, uts: list(ConstructorMap.variant(t))) => {
  uts
  |> List.map(to_variant(ctx))
  |> ListUtil.dedup_f(
       (x: ConstructorMap.variant(t), y: ConstructorMap.variant(t)) =>
       switch (x, y) {
       | (Variant(c1, _, _), Variant(c2, _, _)) => c1 == c2
       | (Variant(_), BadEntry(_))
       | (BadEntry(_), Variant(_))
       | (BadEntry(_), BadEntry(_)) => false
       }
     );
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

let rec subst = (s: t, x: string, ty: t) => {
  let (term, rewrap) = unwrap(ty);
  switch (term) {
  | Int => Int |> rewrap
  | Float => Float |> rewrap
  | Bool => Bool |> rewrap
  | String => String |> rewrap
  | Unknown(prov) => Unknown(prov) |> rewrap
  | Arrow(ty1, ty2) => Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
  | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
  | Sum(sm) =>
    Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
  | Rec({term: Var(y), _} as tp, ty) when x == y => Rec(tp, ty) |> rewrap
  | Rec(y, ty) => Rec(y, subst(s, x, ty)) |> rewrap
  | List(ty) => List(subst(s, x, ty)) |> rewrap
  | Var(y) => x == y ? s : Var(y) |> rewrap
  | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
  | Ap(t1, t2) => Ap(subst(s, x, t1), subst(s, x, t2)) |> rewrap
  };
};

let unroll = (ty: t): t =>
  switch (term_of(ty)) {
  | Rec({term: Var(x), _}, ty_body) => subst(ty, x, ty_body)
  | _ => ty
  };

/* Type Equality: At the moment, this coincides with alpha equivalence,
   but this will change when polymorphic types are implemented */
let rec eq = (t1: t, t2: t): bool => {
  switch (term_of(t1), term_of(t2)) {
  | (Parens(t1), _) => eq(t1, t2)
  | (_, Parens(t2)) => eq(t1, t2)
  | (Rec({term: Var(x1), _}, t1), Rec({term: Var(x2), _}, t2)) =>
    eq(t1, subst(fresh(Var(x1)), x2, t2))
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
  | (Sum(sm1), Sum(sm2)) => ConstructorMap.equal(eq, sm1, sm2)
  | (Sum(_), _) => false
  | (Var(n1), Var(n2)) => n1 == n2
  | (Var(_), _) => false
  | (Ap(t1, t2), Ap(t3, t4)) => eq(t1, t3) && eq(t2, t4)
  | (Ap(_), _) => false
  };
};

let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
  switch (term_of(ty)) {
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => []
  | Var(v) => List.mem(v, bound) ? [] : [v]
  | Parens(ty)
  | List(ty) => free_vars(~bound, ty)
  | Ap(t1, t2)
  | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
  | Sum(sm) => ConstructorMap.free_variables(free_vars(~bound), sm)
  | Prod(tys) => ListUtil.flat_map(free_vars(~bound), tys)
  | Rec({term: Var(x), _}, ty) => free_vars(~bound=[x, ...bound], ty)
  | Rec(_, ty) => free_vars(~bound, ty)
  };

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
    Some(Unknown(join_type_provenance(p1, p2)) |> fresh)
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
  | (Rec({term: Var(x1), _} as tp1, ty1), Rec({term: Var(x2), _}, ty2)) =>
    /* TODO:
         This code isn't fully correct, as we may be doing
         substitution on open terms; if x1 occurs in ty2,
         we should be substituting x1 for a fresh variable
         in ty2. This is annoying, and should be obviated
         by the forthcoming debruijn index implementation
       */
    let ctx = Ctx.extend_dummy_tvar(ctx, x1);
    let+ ty_body =
      join(~resolve, ~fix, ctx, ty1, subst(Var(x1) |> fresh, x2, ty2));
    Rec(tp1, ty_body) |> fresh;
  | (Rec(_), _) => None
  | (Int, Int) => Some(Int |> fresh)
  | (Int, _) => None
  | (Float, Float) => Some(Float |> fresh)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool |> fresh)
  | (Bool, _) => None
  | (String, String) => Some(String |> fresh)
  | (String, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = join'(ty1, ty1');
    let+ ty2 = join'(ty2, ty2');
    Arrow(ty1, ty2) |> fresh;
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    let* tys = ListUtil.map2_opt(join', tys1, tys2);
    let+ tys = OptUtil.sequence(tys);
    Prod(tys) |> fresh;
  | (Prod(_), _) => None
  | (Sum(sm1), Sum(sm2)) =>
    let+ sm' = ConstructorMap.join(eq, join(~resolve, ~fix, ctx), sm1, sm2);
    Sum(sm') |> fresh;
  | (Sum(_), _) => None
  | (List(ty1), List(ty2)) =>
    let+ ty = join'(ty1, ty2);
    List(ty) |> fresh;
  | (List(_), _) => None
  | (Ap(_), _) => failwith("Type join of ap")
  };
};

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by maching against t2 */
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
  | (Sum(sm1), Sum(sm2)) =>
    let sm' = ConstructorMap.match_synswitch(match_synswitch, sm1, sm2);
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
  | Parens(t) => t
  | List(t) => List(normalize(ctx, t)) |> rewrap
  | Ap(t1, t2) => Ap(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Arrow(t1, t2) =>
    Arrow(normalize(ctx, t1), normalize(ctx, t2)) |> rewrap
  | Prod(ts) => Prod(List.map(normalize(ctx), ts)) |> rewrap
  | Sum(ts) =>
    Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts)) |> rewrap
  | Rec({term: Var(name), _} as tpat, ty) =>
    /* NOTE: Dummy tvar added has fake id but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(tpat, normalize(Ctx.extend_dummy_tvar(ctx, name), ty)) |> rewrap
  | Rec(tpat, ty) => Rec(tpat, normalize(ctx, ty)) |> rewrap
  };
};

let matched_arrow = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Arrow(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(SynSwitch) => (
      Unknown(SynSwitch) |> fresh,
      Unknown(SynSwitch) |> fresh,
    )
  | _ => (Unknown(Internal) |> fresh, Unknown(Internal) |> fresh)
  };

let matched_prod = (ctx, length, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Prod(tys) when List.length(tys) == length => tys
  | Unknown(SynSwitch) => List.init(length, _ => Unknown(SynSwitch) |> fresh)
  | _ => List.init(length, _ => Unknown(Internal) |> fresh)
  };

let matched_list = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | List(ty) => ty
  | Unknown(SynSwitch) => Unknown(SynSwitch) |> fresh
  | _ => Unknown(Internal) |> fresh
  };

let matched_args = (ctx, default_arity, ty) => {
  let ty' = weak_head_normalize(ctx, ty);
  switch (term_of(ty')) {
  | Prod([_, ..._] as tys) => tys
  | Unknown(_) => List.init(default_arity, _ => ty')
  | _ => [ty']
  };
};

let get_sum_constructors = (ctx: Ctx.t, ty: t): option(sum_map) => {
  let ty = weak_head_normalize(ctx, ty);
  switch (term_of(ty)) {
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

let is_unknown = (ty: t): bool =>
  switch (ty |> term_of) {
  | Unknown(_) => true
  | _ => false
  };
