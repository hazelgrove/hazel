let matches = (ctx: Contexts.t, t: TPat.t, _ty: HTyp.t, k: Kind.t): Contexts.t => {
  switch (t) {
  | EmptyHole => ctx
  | TyVar(NotInHole, name) => Contexts.push_tyvar(ctx, name, k)
  | TyVar(InHole(_), _) => ctx
  };
};

let fix_holes =
    (ctx: Contexts.t, tp: TPat.t, k: Kind.t, u_gen: MetaVarGen.t)
    : (Contexts.t, TPat.t, MetaVarGen.t) => {
  switch (tp) {
  | EmptyHole => (ctx, EmptyHole, u_gen)
  | TyVar(_, name) =>
    switch (TyTextShape.of_string(name)) {
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ctx, TyVar(InHole(InvalidName, u), name), u_gen);
    | Some(Int | Bool | Float) =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ctx, TyVar(InHole(BuiltinType, u), name), u_gen);
    | Some(ExpandingKeyword(_)) =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ctx, TyVar(InHole(ReservedKeyword, u), name), u_gen);
    | Some(TyVar(_)) =>
      if (TyVar.valid_name(name)) {
        let ctx = Contexts.push_tyvar(ctx, name, k);
        (ctx, TyVar(NotInHole, name), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (ctx, TyVar(InHole(InvalidName, u), name), u_gen);
      }
    }
  };
};

let fix_holes_z =
    (ctx: Contexts.t, zp: ZTPat.t, k: Kind.t, u_gen: MetaVarGen.t)
    : (Contexts.t, ZTPat.t, MetaVarGen.t) => {
  let path = CursorPath_TPat.of_z(zp);
  let (ctx, new_p, u_gen) = fix_holes(ctx, ZTPat.erase(zp), k, u_gen);
  let zp =
    CursorPath_TPat.follow(path, new_p)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ctx, zp, u_gen);
};
