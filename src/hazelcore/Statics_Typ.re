let rec fix_holes =
        (
          ctx: TyVarCtx.t,
          ty: HTyp.t,
          ~renumber_empty_holes: bool=false,
          u_gen: MetaVarGen.t,
        )
        : (HTyp.t, MetaVarGen.t) => {
  let fold_fix_holes = (tys, u_gen) =>
    List.fold_left(
      ((tys', u_gen), ty) => {
        let (ty', u_gen) = fix_holes(ctx, ty, ~renumber_empty_holes, u_gen);
        ([ty', ...tys'], u_gen);
      },
      ([], u_gen),
      tys,
    );
  switch (ty) {
  | TyVarHole(_, u, name) =>
    if (TyVar.Name.(valid(to_string(name))) && TyVarCtx.bound(name, ctx)) {
      (ty, u_gen);
    } else {
      let (u, u_gen) =
        renumber_empty_holes ? (u, u_gen) : MetaVarGen.next(u_gen);
      if (TyVar.Name.(reserved(to_string(name)))) {
        (TyVarHole(Reserved, u, name), u_gen);
      } else if (TyVar.Name.(valid(to_string(name)))) {
        (TyVarHole(Unbound, u, name), u_gen);
      } else {
        (TyVarHole(InvalidName, u, name), u_gen);
      };
    }
  | Hole => (ty, u_gen)
  | TyVar(_)
  | Int
  | Float
  | Bool => (ty, u_gen)
  | Arrow(ty1, ty2) =>
    ();
    let (ty1', u_gen) = fix_holes(ctx, ty1, ~renumber_empty_holes, u_gen);
    let (ty2', u_gen) = fix_holes(ctx, ty2, ~renumber_empty_holes, u_gen);
    (Arrow(ty1', ty2'), u_gen);
  | Sum(ty1, ty2) =>
    let (ty1', u_gen) = fix_holes(ctx, ty1, ~renumber_empty_holes, u_gen);
    let (ty2', u_gen) = fix_holes(ctx, ty2, ~renumber_empty_holes, u_gen);
    (Sum(ty1', ty2'), u_gen);
  | Prod(tys) =>
    let (tys', u_gen) = fold_fix_holes(tys, u_gen);
    (Prod(tys'), u_gen);
  | List(ty1) =>
    let (ty1', u_gen) = fix_holes(ctx, ty1, ~renumber_empty_holes, u_gen);
    (List(ty1'), u_gen);
  };
};
