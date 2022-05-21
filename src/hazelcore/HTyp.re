open OptUtil.Syntax;

module Kind = KindSystem.Kind;
module Context = KindSystem.Context;

module Syntax = KindSystem.HTyp;

/* HTyp */

[@deriving sexp]
type t = Syntax.abs;

/* HTyp Conversions */

let to_string = (ty: t): string => {
  switch (ty) {
  | Hole => "a"
  | Int => "an Integer"
  | Float => "a Float"
  | Bool => "a Boolean"
  | Arrow(_, _) => "a Function"
  | Sum(_, _) => "a Sum"
  | Prod(_) => "a Product"
  | List(_) => "a List"
  | TyVar(_, name) => "a " ++ name
  | TyVarHole(_) => "a"
  };
};

let to_syntax = ty => ty;
let of_syntax = ty => ty;

/* HTyp Constructors */

let hole: t = Hole;
let int: t = Int;
let float: t = Float;
let bool: t = Bool;
let arrow = (ty1: t, ty2: t): t => Arrow(ty1, ty2);
let sum = (tyL: t, tyR: t): t => Sum(tyL, tyR);
let product = (tys: list(t)): t => Prod(tys);
let list = (ty: t): t => List(ty);
let tyvar = (i: Index.Abs.t, name: string): t => TyVar(i, name);
let tyvarhole =
    (reason: TyVarErrStatus.HoleReason.t, u: MetaVar.t, name: string): t =>
  TyVarHole(reason, u, name);

/* HTyp Value Predicates */

let is_hole = (ty: t): bool => ty == Hole;

let is_tyvar = (ty: t): bool =>
  switch (ty) {
  | TyVar(_) => true
  | _ => false
  };

/* Properties of HTyp */

let equivalent: (Context.t, t, t) => bool = Syntax.equivalent;

let rec consistent = (ctx: Context.t, ty: t, ty': t): bool =>
  switch (ty, ty') {
  | (TyVar(i, _), TyVar(i', _)) =>
    switch (Context.tyvar_kind(ctx, i), Context.tyvar_kind(ctx, i')) {
    | (Some(k), Some(k')) =>
      consistent(ctx, Kind.to_htyp(k), Kind.to_htyp(k'))
    | (None, _)
    | (_, None) => false
    }
  | (TyVar(_) | TyVarHole(_) | Hole, _)
  | (_, TyVar(_) | TyVarHole(_) | Hole) => true
  | (Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    consistent(ctx, ty1, ty1') && consistent(ctx, ty2, ty2')
  | (Arrow(_) | Sum(_), _) => false
  | (Prod(tys), Prod(tys')) => List.for_all2(consistent(ctx), tys, tys')
  | (Prod(_), _) => false
  | (List(ty1), List(ty1')) => consistent(ctx, ty1, ty1')
  | (List(_), _) => false
  };

let inconsistent = (ctx: Context.t, ty1: t, ty2: t): bool =>
  !consistent(ctx, ty1, ty2);

let rec consistent_all = (ctx: Context.t, types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(inconsistent(ctx, hd), tl) || consistent_all(ctx, tl)
  };

/* complete (i.e. does not have any holes) */
let rec complete: t => bool =
  fun
  | Hole
  | TyVarHole(_) => false
  | TyVar(_)
  | Int
  | Float
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

/* HTyp Constructor Precedence */

let precedence_Prod = Operators_Typ.precedence(Prod);
let precedence_Arrow = Operators_Typ.precedence(Arrow);
let precedence_Sum = Operators_Typ.precedence(Sum);
let precedence_const = Operators_Typ.precedence_const;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | Hole
  | Prod([])
  | List(_)
  | TyVar(_)
  | TyVarHole(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

/* Type Variables */

let tyvar_index = (ty: t): option(Index.Abs.t) =>
  switch (ty) {
  | TyVar(i, _) => Some(i)
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(_)
  | Sum(_)
  | Prod(_)
  | List(_) => None
  };

let tyvar_name = (ty: t): option(string) =>
  switch (ty) {
  | TyVar(_, name) => Some(name)
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(_)
  | Sum(_)
  | Prod(_)
  | List(_) => None
  };

let subst_tyvar: (t, Index.Abs.t, t) => t = Syntax.subst_tyvar;
let subst_tyvars: (t, list((Index.Abs.t, t))) => t = Syntax.subst_tyvars;

/* Joins */

[@deriving sexp]
type join =
  | GLB
  | LUB;

let rec join = (ctx: Context.t, j: join, ty1: t, ty2: t): option(t) =>
  switch (ty1, ty2) {
  | (TyVarHole(_), TyVarHole(_)) => Some(Hole)
  | (ty, Hole | TyVarHole(_))
  | (Hole | TyVarHole(_), ty) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty)
    }
  | (TyVar(i, _), _) =>
    let* k = Context.tyvar_kind(ctx, i);
    switch (k) {
    | S(ty) => join(ctx, j, ty, ty2)
    | Hole => join(ctx, j, Hole, ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    let* k = Context.tyvar_kind(ctx, i);
    switch (k) {
    | S(ty) => join(ctx, j, ty1, ty)
    | Hole => join(ctx, j, ty1, Hole)
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int | Float | Bool, _) => ty1 == ty2 ? Some(ty1) : None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = join(ctx, j, ty1, ty1');
    let+ ty2 = join(ctx, j, ty2, ty2');
    Syntax.Arrow(ty1, ty2);
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    let* ty1 = join(ctx, j, ty1, ty1');
    let+ ty2 = join(ctx, j, ty2, ty2');
    Syntax.Sum(ty1, ty2);
  | (Prod(tys1), Prod(tys2)) =>
    let+ joined_tys =
      List.map2(join(ctx, j), tys1, tys2) |> OptUtil.sequence;
    Syntax.Prod(joined_tys);
  | (List(ty), List(ty')) =>
    let+ ty = join(ctx, j, ty, ty');
    Syntax.List(ty);
  | (Arrow(_) | Sum(_) | Prod(_) | List(_), _) => None
  };

let join_all = (ctx: Context.t, j: join, types: list(t)): option(t) =>
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    !consistent_all(ctx, types)
      ? None
      : List.fold_left(
          (common_opt, ty) => {
            let* common_ty = common_opt;
            join(ctx, j, common_ty, ty);
          },
          Some(hd),
          tl,
        )
  };

/* HTyp Normalization */

type normalized = Syntax.abs;

let of_normalized: normalized => t = t => t;

/* Replaces every singleton-kinded type variable with a normalized type. */
let rec normalize = (ctx: Context.t, ty: t): normalized => {
  print_endline("HTYP normalize");
  print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(ty)));
  print_endline(Sexplib.Sexp.to_string_hum(Context.sexp_of_t(ctx)));
  switch (ty) {
  | TyVar(i, _) =>
    switch (Context.tyvar_kind(ctx, i)) {
    | Some(S(ty1)) => normalize(ctx, ty1)
    | Some(_) => ty
    | None =>
      failwith(
        __LOC__ ++ ": unknown type variable index " ++ Index.to_string(i),
      )
    }
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => ty
  | Arrow(ty1, ty2) => Arrow(normalize(ctx, ty1), normalize(ctx, ty2))
  | Sum(ty1, ty2) => Sum(normalize(ctx, ty1), normalize(ctx, ty2))
  | Prod(tys) => Prod(List.map(normalize(ctx), tys))
  | List(ty1) => List(normalize(ctx, ty1))
  };
};

/* Properties of Normalized HTyp */

let rec normalized_consistent = (ty: normalized, ty': normalized): bool =>
  switch (ty, ty') {
  | (TyVar(i, _), TyVar(i', _)) =>
    // normalization eliminates all type variables of singleton kind, so these
    // must be of kind Type or KHole
    Index.equal(i, i')
  | (TyVar(_) | TyVarHole(_) | Hole, _)
  | (_, TyVar(_) | TyVarHole(_) | Hole) => true
  | (Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    normalized_consistent(ty1, ty1') && normalized_consistent(ty2, ty2')
  | (Arrow(_) | Sum(_), _) => false
  | (Prod(tys), Prod(tys')) =>
    List.for_all2(normalized_consistent, tys, tys')
  | (Prod(_), _) => false
  | (List(ty1), List(ty1')) => normalized_consistent(ty1, ty1')
  | (List(_), _) => false
  };

let rec normalized_equivalent = (ty: normalized, ty': normalized): bool =>
  switch (ty, ty') {
  | (TyVar(i, _), TyVar(i', _)) => Index.equal(i, i')
  | (TyVar(_), _) => false
  | (TyVarHole(_, u, _), TyVarHole(_, u', _)) => MetaVar.eq(u, u')
  | (TyVarHole(_, _, _), _) => false
  | (Hole | Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    normalized_equivalent(ty1, ty1') && normalized_equivalent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    List.for_all2(normalized_equivalent, tys1, tys2)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => normalized_equivalent(ty, ty')
  | (List(_), _) => false
  };

/* Ground Cases */

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(t) /* the argument is the corresponding ground type */;

let grounded_Arrow = NotGroundOrHole(arrow(hole, hole));
let grounded_Sum = NotGroundOrHole(sum(hole, hole));
let grounded_Prod = length =>
  NotGroundOrHole(product(ListUtil.replicate(length, hole)));
let grounded_List = NotGroundOrHole(list(hole));

let ground_cases_of = (ty: normalized): ground_cases =>
  switch (ty) {
  | TyVarHole(_)
  | Hole => Hole
  | Bool
  | Int
  | Float
  | Arrow(Hole, Hole)
  | Sum(Hole, Hole)
  | List(Hole) => Ground
  | TyVar(_) => Ground
  | Prod(tys) =>
    let equiv = ty => normalized_equivalent(Hole, ty);
    List.for_all(equiv, tys) ? Ground : tys |> List.length |> grounded_Prod;
  | Arrow(_, _) => grounded_Arrow
  | Sum(_, _) => grounded_Sum
  | List(_) => grounded_List
  };

/* HTyp Head-Normalization */

type head_normalized =
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t)
  | TyVar(Index.Abs.t, TyVar.t)
  | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t);

let of_head_normalized: head_normalized => t =
  fun
  | TyVar(i, name) => TyVar(i, name)
  | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) => Arrow(ty1, ty2)
  | Sum(tyL, tyR) => Sum(tyL, tyR)
  | Prod(tys) => Prod(tys)
  | List(ty) => List(ty);

/* Replaces a singleton-kinded type variable with a head-normalized type. */
let rec head_normalize = (ctx: Context.t, ty: t): head_normalized =>
  switch (ty) {
  | TyVar(idx, t) =>
    switch (Context.tyvar_kind(ctx, idx)) {
    | Some(S(ty1)) => head_normalize(ctx, ty1)
    | Some(_) => TyVar(idx, t)
    | None =>
      failwith(
        __LOC__ ++ ": unknown type variable index " ++ Index.to_string(idx),
      )
    }
  | TyVarHole(reason, u, t) => TyVarHole(reason, u, t)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) => Arrow(ty1, ty2)
  | Sum(tyL, tyR) => Sum(tyL, tyR)
  | Prod(tys) => Prod(tys)
  | List(ty) => List(ty)
  };

/* Matched Type Constructors */

let matched_arrow = (ctx: Context.t, ty: t): option((t, t)) =>
  switch (head_normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_)
  | TyVar(_) => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None
  };

let matched_sum = (ctx: Context.t, ty: t): option((t, t)) =>
  switch (head_normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_)
  | TyVar(_) => Some((Hole, Hole))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None
  };

let matched_list = (ctx: Context.t, ty: t): option(t) =>
  switch (head_normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_)
  | TyVar(_) => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None
  };

/* Product Types */

let get_prod_elements: head_normalized => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [of_head_normalized(ty)];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;
