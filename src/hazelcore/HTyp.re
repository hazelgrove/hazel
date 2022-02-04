/** Types with holes */
include HTypCore;

type join =
  | GLB
  | LUB;

let rec normalize = (ctx: TyCtx.t, ty: t): t =>
  switch (ty) {
  | TyVar(i, _) =>
    switch (ctx |> TyCtx.var_kind(i)) {
    | Some(Singleton(_, ty1)) => normalize(ctx, ty1)
    | Some(_) => ty
    | None => failwith(__LOC__ ++ ": unknown type variable index")
    }
  | TyVarHole(_, u, _) =>
    SexpUtil.print(~at="XXX", sexp_of_t(ty));
    switch (ctx |> TyCtx.hole_kind(u)) {
    | Some(Singleton(_, ty1)) => normalize(ctx, ty1)
    | Some(_) => ty
    | None => failwith(__LOC__ ++ ": unknown type variable hole index")
    };
  | Hole => ty
  | Int
  | Float
  | Bool => ty
  | Arrow(ty1, ty2) => Arrow(normalize(ctx, ty1), normalize(ctx, ty2))
  | Sum(ty1, ty2) => Sum(normalize(ctx, ty1), normalize(ctx, ty2))
  | Prod(tys) => Prod(List.map(normalize(ctx), tys))
  | List(ty1) => List(normalize(ctx, ty1))
  };

// assumes ty, ty' have been normalized
let rec normalized_equivalent = (ty: t, ty': t): bool =>
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
let equivalent = (ctx: TyCtx.t, ty: t, ty': t): bool => {
  let ty = normalize(ctx, ty);
  let ty' = normalize(ctx, ty');
  normalized_equivalent(ty, ty');
};

let rec normalized_consistent = (ty: t, ty': t): bool =>
  switch (ty, ty') {
  | (TyVar(i, _), TyVar(i', _)) => Index.equal(i, i')
  | (TyVar(_), _) => false
  // TODO: (eric) is a type variable holes consistent with everything (like an empty type hole) or itself (like a type variable hole)? check the theorems
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

/** Type consistency */
let consistent = (ctx: TyCtx.t, ty: t, ty': t) => {
  let ty = normalize(ctx, ty);
  let ty' = normalize(ctx, ty');
  normalized_consistent(ty, ty');
};

let inconsistent = (ctx: TyCtx.t, ty1: t, ty2: t) =>
  !consistent(ctx, ty1, ty2);

let rec consistent_all = (types: list(t), ctx: TyCtx.t): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(inconsistent(ctx, hd), tl) || consistent_all(tl, ctx)
  };

let rec join = (ctx: TyCtx.t, j: join, ty1: t, ty2: t): option(t) => {
  switch (ty1, ty2) {
  | (TyVarHole(_), TyVarHole(_)) => Some(Hole)
  | (_, Hole)
  | (_, TyVarHole(_)) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty1)
    }
  | (Hole, _)
  | (TyVarHole(_), _) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty2)
    }
  | (TyVar(i, _), _) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx |> TyCtx.var_binding(i);
    switch (k) {
    | Singleton(_, ty) => join(ctx, j, ty, ty2)
    | KHole => join(ctx, j, Hole, ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx |> TyCtx.var_binding(i);
    switch (k) {
    | Kind.Singleton(_, ty) => join(ctx, j, ty1, ty)
    | KHole => join(ctx, j, ty1, Hole)
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = join(ctx, j, ty1, ty1');
    let+ ty2 = join(ctx, j, ty2, ty2');
    Arrow(ty1, ty2);
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = join(ctx, j, ty1, ty1');
    let+ ty2 = join(ctx, j, ty2, ty2');
    Sum(ty1, ty2);
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt((ty1, ty2) => join(ctx, j, ty1, ty2), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    open OptUtil.Syntax;
    let+ ty = join(ctx, j, ty, ty');
    List(ty);
  | (List(_), _) => None
  };
};

let join_all = (ctx: TyCtx.t, j: join, types: list(t)): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types, ctx)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => join(ctx, j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};
