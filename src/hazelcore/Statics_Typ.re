let rec syn = (ctx: Contexts.t, ty: HTyp.t): Kind.t =>
  switch (ty) {
  | TyVar(idx, _) =>
    let (_, k) = List.nth(ctx.tyvars, idx);
    k;
  | TyVarHole(_, _)
  | Hole => KHole
  | Int
  | Float
  | Bool
  | Prod([]) => Type
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    ana(ctx, ty1, Kind.Type) && ana(ctx, ty2, Kind.Type) ? Type : KHole
  | Prod([x, ...xs]) => ana(ctx, x, Kind.Type) ? syn(ctx, Prod(xs)) : KHole
  | List(t) => ana(ctx, t, Kind.Type) ? Type : KHole
  }
and ana = (ctx: Contexts.t, ty: HTyp.t, k: Kind.t): bool =>
  Kind.consistent(k, syn(ctx, ty));
