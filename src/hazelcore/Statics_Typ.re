let rec syn = (tyvars: TyVarCtx.t, ty: HTyp.t): option(Kind.t) =>
  switch (ty) {
  | Hole => Some(KHole)
  | TyVar(i, _) =>
    open OptUtil.Syntax;
    let+ _ = TyVarCtx.kind(tyvars, i);
    Kind.Singleton(Type, ty);
  | TyVarHole(_) => Some(KHole)
  | Int
  | Float
  | Bool => Some(Singleton(Type, ty))
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    open OptUtil.Syntax;
    let* () = ana(tyvars, ty1, Kind.Type);
    let+ () = ana(tyvars, ty2, Kind.Type);
    Kind.Singleton(Type, ty);
  | Prod(tys) =>
    open OptUtil.Syntax;
    let+ () =
      List.fold_left(
        (opt, ty) => Option.bind(opt, _ => ana(tyvars, ty, Kind.Type)),
        Some(),
        tys,
      );
    Kind.Type;
  | List(ty1) =>
    open OptUtil.Syntax;
    let+ _ = ana(tyvars, ty1, Kind.Type);
    Kind.Singleton(Type, ty);
  }

and ana = (tyvars: TyVarCtx.t, ty: HTyp.t, _k: Kind.t): option(unit) =>
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
    let+ _ = syn(tyvars, ty);
    ();
  };
