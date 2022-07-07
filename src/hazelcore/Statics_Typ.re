let rec syn = (ctx: Context.t, ty: HTyp.t): option(Kind.t) =>
  switch (HTyp.to_syntax(ty)) {
  | Hole
  | TyVarHole(_)
  | Int
  | Float
  | Bool => Some(Kind.singleton(ty))
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    open OptUtil.Syntax;
    let* () = ana(ctx, HTyp.of_syntax(ty1), Kind.Type);
    let+ () = ana(ctx, HTyp.of_syntax(ty2), Kind.Type);
    Kind.singleton(ty);
  | Prod(tys) =>
    open OptUtil.Syntax;
    let+ () =
      List.fold_left(
        (opt, ty) =>
          Option.bind(opt, _ => ana(ctx, HTyp.of_syntax(ty), Kind.Type)),
        Some(),
        tys,
      );
    Kind.singleton(ty);
  | List(ty1) =>
    open OptUtil.Syntax;
    let+ _ = ana(ctx, HTyp.of_syntax(ty1), Kind.Type);
    Kind.singleton(ty);
  | Forall(_, ty1) =>
    open OptUtil.Syntax;
    let+ () = ana(ctx, HTyp.of_syntax(ty1), Kind.Type);
    Kind.singleton(ty);
  | TyVar(cref, _) => Context.tyvar_kind(ctx, cref)
  }

and ana = (ctx: Context.t, ty: HTyp.t, k: Kind.t): option(unit) =>
  switch (HTyp.to_syntax(ty)) {
  | Hole
  | TyVarHole(_) => Some()
  // subsumption
  | Sum(_)
  | Prod(_)
  | Arrow(_)
  | Int
  | Float
  | Bool
  | List(_)
  | Forall(_)
  | TyVar(_) =>
    open OptUtil.Syntax;
    let* k' = syn(ctx, ty);
    Kind.consistent_subkind(ctx, k', k) ? Some() : None;
  };
