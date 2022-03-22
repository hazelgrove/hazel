open OptUtil.Syntax;

/* escape hatch for unsafe type operations */
type unsafe = HTypSyntax.t;

/* types */
[@deriving sexp]
type t = HTypSyntax.t;

/* head normalized types */
type head_normalized =
  | TyVar(Index.t, string)
  | TyVarHole(TyVar.HoleReason.t, MetaVar.t, string)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

/* normalized types */
type normalized = HTypSyntax.t;

/**
 * Gets the type in string format.
 * Return string
 */
let to_string = (ty: t): string => {
  switch (ty) {
  | Hole => "a"
  | TyVarHole(_) => "a"
  | TyVar(_, name) => "a " ++ name
  | Int => "an Integer"
  | Float => "a Float"
  | Bool => "a Boolean"
  | Arrow(_, _) => "a Function"
  | Sum(_, _) => "a Sum"
  | Prod(_) => "a Product"
  | List(_) => "a List"
  };
};

let of_unsafe: unsafe => t = t => t;
let unsafe: t => unsafe = t => t;

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

let of_normalized: normalized => t = t => t;

[@deriving sexp]
type join =
  | GLB
  | LUB;

let tyvar = (i: Index.t, name: string): t => TyVar(i, name);
let tyvarhole = (reason: TyVar.HoleReason.t, i: Index.t, name: string): t =>
  TyVarHole(reason, i, name);
let hole: t = Hole;
let int: t = Int;
let float: t = Float;
let bool: t = Bool;
let arrow = (ty1: t, ty2: t): t => Arrow(ty1, ty2);
let sum = (tyL: t, tyR: t): t => Sum(tyL, tyR);
let product = (tys: list(t)): t => Prod(tys);
let list = (ty: t): t => List(ty);

let is_hole = (ty: t): bool => ty == Hole;
let is_tyvar = (ty: t): bool =>
  switch (ty) {
  | TyVar(_) => true
  | _ => false
  };

let tyvar_index = (ty: t): option(Index.t) =>
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

/*
 Replaces a singleton-kinded type variable with a head-normalized type.
 */
let rec head_normalize = (tyvars: TyVarCtx.t, ty: t): head_normalized =>
  switch (ty) {
  | TyVar(i, name) =>
    switch (TyVarCtx.kind(tyvars, i)) {
    | Some(Singleton(ty1)) => head_normalize(tyvars, ty1)
    | Some(_) => TyVar(i, name)
    | None =>
      failwith(
        __LOC__ ++ ": unknown type variable index " ++ Int.to_string(i),
      )
    }
  | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(ty1, ty2) => Arrow(ty1, ty2)
  | Sum(tyL, tyR) => Sum(tyL, tyR)
  | Prod(tys) => Prod(tys)
  | List(ty) => List(ty)
  };

/*
 Replaces every singleton-kinded type variable with a normalized type.
 */
let rec normalize = (tyvars: TyVarCtx.t, ty: t): normalized =>
  switch (ty) {
  | TyVar(i, _) =>
    switch (TyVarCtx.kind(tyvars, i)) {
    | Some(Singleton(ty1)) => normalize(tyvars, ty1)
    | Some(_) => ty
    | None =>
      failwith(
        __LOC__ ++ ": unknown type variable index " ++ Int.to_string(i),
      )
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

/* type consistency for normalized types is context independent */
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

/* context-dependent type equivalence */
let equivalent = (tyvars: TyVarCtx.t, ty: t, ty': t): bool =>
  normalized_equivalent(normalize(tyvars, ty), normalize(tyvars, ty'));

let consistent = (tyvars: TyVarCtx.t, ty: t, ty': t): bool =>
  normalized_consistent(normalize(tyvars, ty), normalize(tyvars, ty'));

let inconsistent = (tyvars: TyVarCtx.t, ty1: t, ty2: t): bool =>
  !consistent(tyvars, ty1, ty2);

let rec consistent_all = (tyvars: TyVarCtx.t, types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(inconsistent(tyvars, hd), tl) || consistent_all(tyvars, tl)
  };

let matched_arrow = (tyvars: TyVarCtx.t, ty: t): option((t, t)) =>
  switch (head_normalize(tyvars, ty)) {
  | Hole
  | TyVarHole(_)
  | TyVar(_) => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None
  };

let get_prod_elements: head_normalized => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [of_head_normalized(ty)];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

/* matched sum types */
let matched_sum = (tyvars: TyVarCtx.t, ty: t): option((t, t)) =>
  switch (head_normalize(tyvars, ty)) {
  | Hole
  | TyVarHole(_)
  | TyVar(_) => Some((Hole, Hole))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None
  };

/* matched list types */
let matched_list = (tyvars: TyVarCtx.t, ty: t): option(t) =>
  switch (head_normalize(tyvars, ty)) {
  | Hole
  | TyVarHole(_)
  | TyVar(_) => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None
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
    let* k = TyVarCtx.kind(tyvars, i);
    switch (k) {
    | Singleton(ty) => join(tyvars, j, ty, ty2)
    | KHole => join(tyvars, j, Hole, ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    let* k = TyVarCtx.kind(tyvars, i);
    switch (k) {
    | KindCore.Singleton(ty) => join(tyvars, j, ty1, ty)
    | KHole => join(tyvars, j, ty1, Hole)
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int | Float | Bool, _) => ty1 == ty2 ? Some(ty1) : None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let* ty1 = join(tyvars, j, ty1, ty1');
    let+ ty2 = join(tyvars, j, ty2, ty2');
    HTypSyntax.Arrow(ty1, ty2);
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    let* ty1 = join(tyvars, j, ty1, ty1');
    let+ ty2 = join(tyvars, j, ty2, ty2');
    HTypSyntax.Sum(ty1, ty2);
  | (Prod(tys1), Prod(tys2)) =>
    let+ joined_tys =
      List.map2(join(tyvars, j), tys1, tys2) |> OptUtil.sequence;
    HTypSyntax.Prod(joined_tys);
  | (List(ty), List(ty')) =>
    let+ ty = join(tyvars, j, ty, ty');
    HTypSyntax.List(ty);
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

let subst = HTypSyntax.subst;
