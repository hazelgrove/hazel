// Construction.re encompasses Kinds and Types and Type constructors

open Kind;
/// k1 is a consistent_subkind of k2
let rec consistent_subkind = (ctx, k1, k2) =>
  switch (k1, k2) {
  | (KHole, _)
  | (_, KHole) => true
  | (Singleton(_), Type) => true
  | (k1, k2) when kequiv(ctx, k1, k2) => true
  | (_, _) => false
  }
// k1 is equivalent to k2
and kequiv = (ctx, k1, k2) =>
  switch (k1, k2) {
  | (KHole, KHole)
  | (Type, Type) => true
  | (Singleton(ty1), Singleton(ty2)) when kcequiv(ctx, ty1, ty2, Kind.Type) =>
    true
  | (KHole | Type | Singleton(_), _) => false
  }
// ty1 is equivalent to ty2 and its kind is k
and kcequiv = (ctx, ty1, ty2, k) => {
  switch (ty1, ty2) {
  | (TyVar(idx1, _), TyVar(idx2, _)) when idx1 == idx2 =>
    let (_, k') = TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx1);
    kequiv(ctx, k, k');
  | (TyVar(idx1, _), ty2) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx1)) {
    | (_, Singleton(ty1)) => kcequiv(ctx, ty1, ty2, Kind.Type)
    | (_, KHole | Type) =>
      failwith("impossible for bounded type variables (currently)")
    }
  | (ty1, TyVar(idx2, _)) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx2)) {
    | (_, Singleton(ty2)) => kcequiv(ctx, ty1, ty2, Kind.Type)
    | (_, KHole | Type) =>
      failwith("impossible for bounded type variables (currently)")
    }
  | (TyVarHole(_, id1), TyVarHole(_, id2)) =>
    if (TyId.eq(id1, id2)) {
      kequiv(ctx, k, Kind.KHole);
    } else {
      false;
    }
  // TODO: Make these not equivalent when we add identifiers
  | (Hole, Hole)
  | (Int, Int)
  | (Bool, Bool)
  | (Float, Float) => kequiv(ctx, k, Kind.Type)

  | (Arrow(ty1, ty3), Arrow(ty2, ty4))
  | (Sum(ty1, ty3), Sum(ty2, ty4)) =>
    kcequiv(ctx, ty1, ty3, Kind.Type)
    && kcequiv(ctx, ty2, ty4, Kind.Type)
    && kequiv(ctx, k, Kind.Type)
  | (Prod(ts), Prod(ts')) when List.length(ts) == List.length(ts') =>
    List.combine(ts, ts')
    |> List.for_all(((x, y)) => kcequiv(ctx, x, y, Kind.Type))
  | (List(ty1), List(ty2)) => kcequiv(ctx, ty1, ty2, Kind.Type)
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
};
