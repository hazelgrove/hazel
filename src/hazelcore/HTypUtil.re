// This function is wrong, need to revise. need to look at PFPL chapter 43
/* equality */
let rec eq = (ctx: Contexts.t, x: HTyp.t, y: HTyp.t): bool =>
  switch (x, y) {
  | (TyVar(idx1, _), ty2) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx1)) {
    | (_, Singleton(ty1)) => eq(ctx, ty1, ty2)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | (ty1, TyVar(idx2, _)) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx2)) {
    | (_, Singleton(ty2)) => eq(ctx, ty1, ty2)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | (TyVarHole(_, id1), TyVarHole(_, id2)) => TyId.eq(id1, id2)
  | (Hole, Hole)
  | (Int, Int)
  | (Bool, Bool)
  | (Float, Float) => true
  | (Arrow(ty1, ty2), Arrow(ty3, ty4))
  | (Sum(ty1, ty2), Sum(ty3, ty4)) =>
    eq(ctx, ty1, ty3) && eq(ctx, ty2, ty4)
  | (Prod(lst1), Prod(lst2)) =>
    List.fold_left(
      (b, pair) => {
        let (ty1, ty2) = pair;
        b && eq(ctx, ty1, ty2);
      },
      true,
      List.combine(lst1, lst2),
    )
  | (List(ty1), List(ty2)) => eq(ctx, ty1, ty2)
  | (
      Hole | Int | Bool | Float | Arrow(_, _) | Sum(_, _) | Prod(_) | List(_),
      _,
    )
  | (
      _,
      Hole | Int | Bool | Float | Arrow(_, _) | Sum(_, _) | Prod(_) | List(_),
    ) =>
    false
  };

/* type consistency */
let rec consistent = (ctx: Contexts.t, x: HTyp.t, y: HTyp.t): bool => {
  let _ = TyVarCtx.print(ctx |> Contexts.tyvars);
  switch (x, y) {
  | (Hole, _) => true
  | (_, Hole) => true
  | (TyVarHole(_), _) => true
  | (_, TyVarHole(_)) => true
  | (TyVar(i, _), _) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, i)) {
    | (_, Singleton(hty)) => consistent(ctx, hty, y)
    | _ => failwith(__LOC__)
    }
  | (_, TyVar(i, _)) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, i)) {
    | (_, Singleton(hty)) => consistent(ctx, x, hty)
    | _ => failwith(__LOC__)
    }
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    consistent(ctx, ty1, ty1') && consistent(ctx, ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.for_all2_opt(consistent(ctx), tys1, tys2)
    |> Option.value(~default=false)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => consistent(ctx, ty, ty')
  | (List(_), _) => false
  };
};
let inconsistent = (ctx, ty1, ty2) => !consistent(ctx, ty1, ty2);

let rec consistent_all = (ctx: Contexts.t, types: list(HTyp.t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    if (List.exists(inconsistent(ctx, hd), tl)) {
      false;
    } else {
      consistent_all(ctx, tl);
    }
  };

let rec normalize = (ctx: Contexts.t, ty: HTyp.t): HTyp.t =>
  switch (ty) {
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => ty
  | TyVar(idx, _) =>
    switch (TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx)) {
    | (_, Singleton(hty)) => normalize(ctx, hty)
    | _ => failwith("impossible for bounded variables")
    }
  | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
  | Sum(t1, t2) => Sum(normalize(ctx, t1), normalize(ctx, t2))
  | Prod(lst) => Prod(List.map(t => normalize(ctx, t), lst))
  | List(t) => List(normalize(ctx, t))
  };

/* matched arrow types */
let rec matched_arrow =
        (ctx: Contexts.t, ty: HTyp.t): option((HTyp.t, HTyp.t)) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
  | TyVar(idx, _) =>
    switch (TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx)) {
    | (_, Singleton(hty)) => matched_arrow(ctx, hty)
    | _ => failwith("impossible for bounded variables")
    }
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None
  };

let has_matched_arrow = (ctx: Contexts.t, ty: HTyp.t): bool =>
  switch (normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_)
  | Arrow(_) => true
  | _ => false
  };

/* matched sum types */
let rec matched_sum =
        (ctx: Contexts.t, ty: HTyp.t): option((HTyp.t, HTyp.t)) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
  | TyVar(idx, _) =>
    switch (TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx)) {
    | (_, Singleton(hty)) => matched_sum(ctx, hty)
    | _ => failwith("impossible for bounded variables")
    }
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None
  };

let has_matched_sum = (ctx: Contexts.t, ty: HTyp.t): bool =>
  switch (normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_)
  | Sum(_) => true
  | _ => false
  };

/* matched list types */
let rec matched_list = (ctx: Contexts.t, ty: HTyp.t): option(HTyp.t) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some(Hole)
  | TyVar(idx, _) =>
    switch (TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx)) {
    | (_, Singleton(hty)) => matched_list(ctx, hty)
    | _ => failwith("impossible for bounded variables")
    }
  | List(ty) => Some(ty)
  | _ => None
  };

let has_matched_list = (ctx: Contexts.t, ty: HTyp.t): bool =>
  switch (normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_)
  | List(_) => true
  | _ => false
  };

/* complete (i.e. does not have any holes) */
let rec complete = (ctx: Contexts.t, ty: HTyp.t): bool =>
  switch (ty) {
  | TyVar(idx, _) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx)) {
    | (_, Singleton(ty)) => complete(ctx, ty)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | Hole
  | TyVarHole(_) => false
  | Int
  | Float
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ctx, ty1) && complete(ctx, ty2)
  | Prod(tys) => tys |> List.for_all(complete(ctx))
  | List(ty) => complete(ctx, ty)
  };

let rec join =
        (ctx: Contexts.t, j: HTyp.join, ty1: HTyp.t, ty2: HTyp.t)
        : option(HTyp.t) =>
  switch (ty1, ty2) {
  | (TyVarHole(_), TyVarHole(_)) => Some(Hole)
  | (_, Hole)
  | (_, TyVarHole(_, _)) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty1)
    }
  | (Hole, _)
  | (TyVarHole(_, _), _) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty2)
    }
  | (TyVar(idx1, _), ty2) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx1)) {
    | (_, Singleton(ty1)) => join(ctx, j, ty1, ty2)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | (ty1, TyVar(idx2, _)) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx2)) {
    | (_, Singleton(ty2)) => join(ctx, j, ty1, ty2)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(ctx, j, ty1, ty1'), join(ctx, j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    switch (join(ctx, j, ty1, ty1'), join(ctx, j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt(join(ctx, j), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => HTyp.Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    switch (join(ctx, j, ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join_all =
    (ctx: Contexts.t, j: HTyp.join, types: list(HTyp.t)): option(HTyp.t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(ctx, types)) {
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
