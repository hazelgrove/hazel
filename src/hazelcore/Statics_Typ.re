let rec syn = (ctx: Contexts.t, ty: HTyp.t): option(Kind.t) =>
  switch (HTyp.unsafe(ty)) {
  | TyVar(i, _) => Contexts.tyvar_kind(ctx, i)
  | Hole
  | TyVarHole(_)
  | Int
  | Float
  | Bool => Some(Kind.singleton(ty))
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    open OptUtil.Syntax;
    let* () = ana(ctx, HTyp.of_unsafe(ty1), KindCore.T);
    let+ () = ana(ctx, HTyp.of_unsafe(ty2), KindCore.T);
    Kind.singleton(ty);
  | Prod(tys) =>
    open OptUtil.Syntax;
    let+ () =
      List.fold_left(
        (opt, ty) =>
          Option.bind(opt, _ => ana(ctx, HTyp.of_unsafe(ty), KindCore.T)),
        Some(),
        tys,
      );
    Kind.singleton(ty);
  | List(ty1) =>
    open OptUtil.Syntax;
    let+ _ = ana(ctx, HTyp.of_unsafe(ty1), KindCore.T);
    Kind.singleton(ty);
  }

and ana = (ctx: Contexts.t, ty: HTyp.t, k: Kind.t): option(unit) =>
  switch (HTyp.unsafe(ty)) {
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
    let* k' = syn(ctx, ty);
    Kind.consistent_subkind(ctx, k', k) ? Some() : None;
  };
