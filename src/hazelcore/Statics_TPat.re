let matches = (ctx: Context.t, tp: TPat.t, k: Kind.t): Context.t =>
  switch (tp) {
  | EmptyHole
  | TyVar(InHole(_), _) => ctx
  | TyVar(NotInHole, t) => Context.add_tyvar(ctx, t, k)
  };

let fix_holes =
    (ctx: Context.t, tp: TPat.t, k: Kind.t, id_gen: IDGen.t)
    : (Context.t, TPat.t, IDGen.t) =>
  switch (tp) {
  | EmptyHole => (ctx, EmptyHole, id_gen)
  | TyVar(_, t) =>
    switch (TyTextShape.of_string(t)) {
    | None =>
      let (u, id_gen) = IDGen.next(id_gen);
      (ctx, TyVar(InHole(InvalidName, u), t), id_gen);
    | Some(Int | Bool | Float) =>
      let (u, id_gen) = IDGen.next(id_gen);
      (ctx, TyVar(InHole(BuiltinType, u), t), id_gen);
    | Some(ExpandingKeyword(_)) =>
      let (u, id_gen) = IDGen.next(id_gen);
      (ctx, TyVar(InHole(ReservedKeyword, u), t), id_gen);
    | Some(TyVar(_)) =>
      if (TyVar.valid_name(t)) {
        let ctx = Context.add_tyvar(ctx, t, k);
        (ctx, TyVar(NotInHole, t), id_gen);
      } else {
        let (u, id_gen) = IDGen.next(id_gen);
        (ctx, TyVar(InHole(InvalidName, u), t), id_gen);
      }
    }
  };

let fix_holes_z =
    (ctx: Context.t, ztp: ZTPat.t, k: Kind.t, id_gen: IDGen.t)
    : (Context.t, ZTPat.t, IDGen.t) => {
  let path = CursorPath_TPat.of_z(ztp);
  let (ctx, new_tp, id_gen) = fix_holes(ctx, ZTPat.erase(ztp), k, id_gen);
  let ztp =
    CursorPath_TPat.follow(path, new_tp)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ctx, ztp, id_gen);
};
