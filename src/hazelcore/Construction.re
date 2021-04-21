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
  | (Singleton(ty1), Singleton(ty2))
      when kcequiv(ctx, ty1, ty2) |> Option.is_some =>
    true
  | (_, _) => false
  }
// ty1 is equivalent to ty2 and its kind is k
and kcequiv = (ctx, ty1, ty2) => {
  // joins two kind results
  let join = (k1, k2) =>
    switch (k1, k2) {
    | (None, _)
    | (_, None) => None
    | (Some(k1), Some(k2)) =>
      // TODO: Not exactly sure if this is right
      switch (k1, k2) {
      | (KHole, _)
      | (_, KHole) => Some(KHole)
      | (Type, Type) => Some(Type)
      | (Singleton(_), _)
      | (_, Singleton(_)) =>
        failwith("kcequiv doesn't ever generate a singleton")
      }
    };

  switch (ty1, ty2) {
  | (TyVar(idx1, _), ty2) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx1)) {
    | (_, Singleton(ty1)) => kcequiv(ctx, ty1, ty2)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | (ty1, TyVar(idx2, _)) =>
    switch (TyVarCtx.tyvar_with_idx(ctx |> Contexts.tyvars, idx2)) {
    | (_, Singleton(ty2)) => kcequiv(ctx, ty1, ty2)
    | (_, _) => failwith("impossible for bounded type variables")
    }
  | (TyVarHole(_, id1), TyVarHole(_, id2)) =>
    if (TyId.eq(id1 |> TyId.of_string, id2 |> TyId.of_string)) {
      Some(Kind.KHole);
    } else {
      None;
    }
  | (Hole, Hole)
  | (Int, Int)
  | (Bool, Bool)
  | (Float, Float)
  | (Prod([]), Prod([])) => Some(Kind.Type)
  | (Arrow(ty1, ty2), Arrow(ty3, ty4))
  | (Sum(ty1, ty2), Sum(ty3, ty4)) =>
    join(kcequiv(ctx, ty1, ty3), kcequiv(ctx, ty2, ty4))
  | (Prod([t1, ...lst1]), Prod([t2, ...lst2])) =>
    List.combine(lst1, lst2)
    |> List.map(((x, y)) => kcequiv(ctx, x, y))
    |> List.fold_left(join, kcequiv(ctx, t1, t2))
  | (List(ty1), List(ty2)) => kcequiv(ctx, ty1, ty2)
  | (
      Hole | Int | Bool | Float | Arrow(_, _) | Sum(_, _) | Prod(_) | List(_),
      _,
    )
  | (
      _,
      Hole | Int | Bool | Float | Arrow(_, _) | Sum(_, _) | Prod(_) | List(_),
    ) =>
    None
  };
};
