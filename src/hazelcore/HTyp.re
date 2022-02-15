open OptUtil.Syntax;

include HTypCore;

[@deriving sexp]
type join =
  | GLB
  | LUB;

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

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */
let eq = (==);

/* type variable normalization */
let rec head_normalize = (tyvars: TyVarCtx.t, ty: t): t =>
  switch (ty) {
  | TyVar(i, _) =>
    switch (TyVarCtx.kind(tyvars, i)) {
    | Some(Singleton(_, ty1)) => head_normalize(tyvars, ty1)
    | Some(_) => ty
    | None => failwith(__LOC__ ++ ": unknown type variable index")
    }
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(_, _)
  | Sum(_, _)
  | Prod(_)
  | List(_) => ty
  };

/*
 Type normalization replaces each type variable of singleton kind with the
 (recursively nomalized) type embedded in the kind.
 */
let rec normalize = (tyvars: TyVarCtx.t, ty: t): t =>
  switch (ty) {
  | TyVar(i, _) =>
    switch (TyVarCtx.kind(tyvars, i)) {
    | Some(Singleton(_, ty1)) => normalize(tyvars, ty1)
    | Some(_) => ty
    | None => failwith(__LOC__ ++ ": unbound type variable index")
    }
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => ty
  | Arrow(ty1, ty2) =>
    Arrow(normalize(tyvars, ty1), normalize(tyvars, ty2))
  | Sum(ty1, ty2) => Sum(normalize(tyvars, ty1), normalize(tyvars, ty2))
  | Prod(tys) => Prod(List.map(normalize(tyvars), tys))
  | List(ty1) => List(normalize(tyvars, ty1))
  };

/* type equivalence for normalized types is context independent */
let rec normalized_equivalent = (ty: t, ty': t): bool =>
  switch (ty, ty') {
  | (TyVar(i, _), TyVar(i', _)) => Index.equal(i, i')
  | (TyVar(_), _) => false
  | (TyVarHole(_, u, _), TyVarHole(_, u', _)) => MetaVar.eq(u, u')
  | (TyVarHole(_, _, _), _) => false
  | (Hole | Int | Float | Bool, _) => eq(ty, ty')
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

/* type consistency for normalized types is context independent */
let rec normalized_consistent = (ty: t, ty': t): bool =>
  switch (ty, ty') {
  | (TyVar(i, _), TyVar(i', _)) => Index.equal(i, i')
  | (TyVar(_), _) => false
  | (TyVarHole(_) | Hole, _)
  | (_, TyVarHole(_) | Hole) => true
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

/* context-dependent type equivalence */
let equivalent = (ctx: TyVarCtx.t, ty: t, ty': t): bool => {
  let ty = normalize(ctx, ty);
  let ty' = normalize(ctx, ty');
  normalized_equivalent(ty, ty');
};

/* context-dependent type consistency */
let consistent = (tyvars: TyVarCtx.t, ty: t, ty': t): bool =>
  normalized_consistent(normalize(tyvars, ty), normalize(tyvars, ty'));

let inconsistent = (tyvars: TyVarCtx.t, ty1: t, ty2: t) =>
  !consistent(tyvars, ty1, ty2);

let rec consistent_all = (tyvars: TyVarCtx.t, types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(inconsistent(tyvars, hd), tl) || consistent_all(tyvars, tl)
  };

/* matched arrow types */
let matched_arrow =
  fun
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

/* matched sum types */
let matched_sum =
  fun
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

/* matched list types */
let matched_list =
  fun
  | Hole
  | TyVarHole(_) => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None;

/* complete (i.e. does not have any holes) */
let rec complete =
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

let rec join = (tyvars: TyVarCtx.t, j: join, ty1: t, ty2: t): option(t) =>
  switch (ty1, ty2) {
  | (TyVarHole(_), TyVarHole(_)) => Some(Hole)
  | (ty, Hole | TyVarHole(_))
  | (Hole | TyVarHole(_), ty) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty)
    }
  | (TyVar(i, _), _) =>
    open OptUtil.Syntax;
    let* k = TyVarCtx.kind(tyvars, i);
    switch (k) {
    | Singleton(_, ty) => join(tyvars, j, ty, ty2)
    | KHole => join(tyvars, j, Hole, ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    open OptUtil.Syntax;
    let* k = TyVarCtx.kind(tyvars, i);
    switch (k) {
    | Kind.Singleton(_, ty) => join(tyvars, j, ty1, ty)
    | KHole => join(tyvars, j, ty1, Hole)
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int | Float | Bool, _) => eq(ty1, ty2) ? Some(ty1) : None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = join(tyvars, j, ty1, ty1');
    let+ ty2 = join(tyvars, j, ty2, ty2');
    Arrow(ty1, ty2);
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    let* ty1 = join(tyvars, j, ty1, ty1');
    let+ ty2 = join(tyvars, j, ty2, ty2');
    Sum(ty1, ty2);
  | (Prod(tys1), Prod(tys2)) =>
    let+ joined_tys =
      List.map2(join(tyvars, j), tys1, tys2) |> OptUtil.sequence;
    Prod(joined_tys);
  | (List(ty), List(ty')) =>
    let+ ty = join(tyvars, j, ty, ty');
    List(ty);
  | (Arrow(_) | Sum(_) | Prod(_) | List(_), _) => None
  };

let join_all = (tyvars: TyVarCtx.t, j: join, types: list(t)): option(t) =>
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    !consistent_all(tyvars, types)
      ? None
      : List.fold_left(
          (common_opt, ty) => {
            let* common_ty = common_opt;
            join(tyvars, j, common_ty, ty);
          },
          Some(hd),
          tl,
        )
  };
