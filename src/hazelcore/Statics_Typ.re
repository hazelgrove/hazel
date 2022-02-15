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

// let syn_syn_fix_holes = (tyvars:TyVarCtx.t, t:HTyp.t, u_gen:MetaVarGen.t):

let rec syn_fix_holes =
        (
          tyvars: TyVarCtx.t,
          ty: HTyp.t,
          ~renumber_empty_holes: bool=false,
          u_gen: MetaVarGen.t,
        )
        : (HTyp.t, MetaVarGen.t) => {
  let fold_syn_fix_holes = (tys, u_gen) =>
    List.fold_left(
      ((tys', u_gen), ty) => {
        let (ty', u_gen) =
          syn_fix_holes(tyvars, ty, ~renumber_empty_holes, u_gen);
        ([ty', ...tys'], u_gen);
      },
      ([], u_gen),
      tys,
    );
  switch (ty) {
  | Hole => (ty, u_gen)
  | TyVarHole(_, u, name) =>
    switch (name) {
    | _ when TyVarCtx.has_name(tyvars, name) =>
      switch (TyVarCtx.index(tyvars, name)) {
      | Some(i) => (TyVar(i, name), u_gen)
      | None => failwith(__LOC__ ++ ": impossible case clause")
      }
    | _ when TyVar.reserved_word(name) => (
        TyVarHole(Reserved, u, name),
        u_gen,
      )
    | _ when TyVar.valid_name(name) => (TyVarHole(Unbound, u, name), u_gen)
    | _ =>
      let ty: HTyp.t =
        switch (name) {
        | _ when TyVar.reserved_word(name) => TyVarHole(Reserved, u, name)
        | _ when TyVar.valid_name(name) => TyVarHole(Unbound, u, name)
        | _ => TyVarHole(InvalidName, u, name)
        };
      (ty, u_gen);
    }
  | TyVar(i, _) when TyVarCtx.has_index(tyvars, i) => (ty, u_gen)
  | TyVar(_, name) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (TyVarHole(Unbound, u, name), u_gen);
  | Int
  | Float
  | Bool => (ty, u_gen)
  | Arrow(ty1, ty2) =>
    ();
    let (ty1', u_gen) =
      syn_fix_holes(tyvars, ty1, ~renumber_empty_holes, u_gen);
    let (ty2', u_gen) =
      syn_fix_holes(tyvars, ty2, ~renumber_empty_holes, u_gen);
    (Arrow(ty1', ty2'), u_gen);
  | Sum(ty1, ty2) =>
    let (ty1', u_gen) =
      syn_fix_holes(tyvars, ty1, ~renumber_empty_holes, u_gen);
    let (ty2', u_gen) =
      syn_fix_holes(tyvars, ty2, ~renumber_empty_holes, u_gen);
    (Sum(ty1', ty2'), u_gen);
  | Prod(tys) =>
    let (tys', u_gen) = fold_syn_fix_holes(tys, u_gen);
    (Prod(tys'), u_gen);
  | List(ty1) =>
    let (ty1', u_gen) =
      syn_fix_holes(tyvars, ty1, ~renumber_empty_holes, u_gen);
    (List(ty1'), u_gen);
  };
};
