let rec syn = (tyvars: TyVarCtx.t, ty: HTyp.t): option(Kind.t) =>
  switch (ty) {
  | Hole => Some(KHole)
  | TyVar(i, _) => TyVarCtx.kind(tyvars, i)
  | TyVarHole(_) => Some(KHole)
  | Int
  | Float
  | Bool => Some(Singleton(ty))
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    open OptUtil.Syntax;
    let* () = ana(tyvars, ty1, Kind.Type);
    let+ () = ana(tyvars, ty2, Kind.Type);
    Kind.Singleton(ty);
  | Prod(tys) =>
    open OptUtil.Syntax;
    let+ () =
      List.fold_left(
        (opt, ty) => Option.bind(opt, _ => ana(tyvars, ty, Kind.Type)),
        Some(),
        tys,
      );
    Kind.Singleton(ty);
  | List(ty1) =>
    open OptUtil.Syntax;
    let+ _ = ana(tyvars, ty1, Kind.Type);
    Kind.Singleton(ty);
  }

and ana = (tyvars: TyVarCtx.t, ty: HTyp.t, k: Kind.t): option(unit) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some()
  // subsumption
  | Sum(_)
  | Prod(_)
  | TyVar(_)
  | Arrow(_)
  | Int
  | Float
  | Bool
  | List(_) =>
    open OptUtil.Syntax;
    let* k' = syn(tyvars, ty);
    Kind.consistent_subkind(tyvars, k', k) ? Some() : None;
  };
