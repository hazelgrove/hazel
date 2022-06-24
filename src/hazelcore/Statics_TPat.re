let matches = (ctx: Context.t, tp: TPat.t, _ty: HTyp.t, k: Kind.t): Context.t => {
  switch (tp) {
  | EmptyHole => ctx
  | TyVar(NotInHole, t) => Context.add_tyvar(ctx, t, k)
  | TyVar(InHole(_), _) => ctx
  };
};

let fix_holes =
    (ctx: Context.t, tp: TPat.t, k: Kind.t, u_gen: MetaVarGen.t)
    : (Context.t, TPat.t, MetaVarGen.t) => {
  switch (tp) {
  | EmptyHole => (ctx, EmptyHole, u_gen)
  | TyVar(_, t) =>
    switch (TyTextShape.of_string(t)) {
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ctx, TyVar(InHole(InvalidName, u), t), u_gen);
    | Some(Int | Bool | Float) =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ctx, TyVar(InHole(BuiltinType, u), t), u_gen);
    | Some(ExpandingKeyword(_)) =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ctx, TyVar(InHole(ReservedKeyword, u), t), u_gen);
    | Some(TyVar(_)) =>
      if (TyVar.valid_name(t)) {
        let ctx = Context.add_tyvar(ctx, t, k);
        (ctx, TyVar(NotInHole, t), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (ctx, TyVar(InHole(InvalidName, u), t), u_gen);
      }
    }
  };
};

let fix_holes_z =
    (ctx: Context.t, ztp: ZTPat.t, k: Kind.t, u_gen: MetaVarGen.t)
    : (Context.t, ZTPat.t, MetaVarGen.t) => {
  let path = CursorPath_TPat.of_z(ztp);
  let (ctx, new_tp, u_gen) = fix_holes(ctx, ZTPat.erase(ztp), k, u_gen);
  let ztp =
    CursorPath_TPat.follow(path, new_tp)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ctx, ztp, u_gen);
};
