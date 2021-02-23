let rec syn = (ctx: Contexts.t, ty: HTyp.t): option(Kind.t) =>
  switch (ty) {
  | TyVar(idx, _) =>
    let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx);
    Some(k);
  | TyVarHole(_, _)
  | Hole => Some(KHole)
  | Int
  | Float
  | Bool
  | Prod([]) => Some(Type)
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    ana(ctx, ty1, Kind.Type) && ana(ctx, ty2, Kind.Type) ? Some(Type) : None
  | Prod(lst) =>
    List.fold_left(
      (b1, b2) => b1 && b2,
      true,
      List.map(hty => ana(ctx, hty, Kind.Type), lst),
    )
      ? Some(Type) : None
  | List(t) => ana(ctx, t, Kind.Type) ? Some(Type) : None
  }
and ana = (ctx: Contexts.t, ty: HTyp.t, k: Kind.t): bool =>
  switch (syn(ctx, ty)) {
  | None => false
  | Some(syn_k) => Kind.consistent(k, syn_k)
  };
