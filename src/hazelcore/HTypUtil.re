let rec eq = (ctx: Contexts.t, x: HTyp.t, y: HTyp.t): bool =>
  switch (x, y) {
  | (TyVar(id1, _), TyVar(id2, _)) =>
    switch (
      TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, id1),
      TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, id2),
    ) {
    | ((_, Singleton(h1)), (_, Singleton(h2))) => eq(ctx, h1, h2)
    | _ => failwith(__LOC__)
    }
  | (_, _) => x == y
  };

let rec consistent = (ctx: Contexts.t, x: HTyp.t, y: HTyp.t): bool => {
  let _ = print_endline("consisten begin");
  let _ = print_endline(x |> HTyp.print);
  let _ = print_endline(y |> HTyp.print);
  let _ = print_endline("print end");
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
let matched_arrow = (ctx: Contexts.t, ty: HTyp.t): option((HTyp.t, HTyp.t)) =>
  switch (normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
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
let matched_sum = (ctx: Contexts.t, ty: HTyp.t): option((HTyp.t, HTyp.t)) =>
  switch (normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_) => Some((Hole, Hole))
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
let matched_list = (ctx: Contexts.t, ty: HTyp.t): option(HTyp.t) =>
  switch (normalize(ctx, ty)) {
  | Hole
  | TyVarHole(_) => Some(Hole)
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
