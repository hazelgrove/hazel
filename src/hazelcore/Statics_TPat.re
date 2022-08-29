let ana = Context.add_tpat;

let ana_fix_holes =
    (ctx: Context.t, tp: TPat.t, k: Kind.t, id_gen: IDGen.t)
    : (Context.t, TPat.t, IDGen.t) =>
  switch (tp) {
  | EmptyHole => (ctx, EmptyHole, id_gen)
  | InvalidText(_, t)
  | TyVar(_, t) =>
    switch (TyTextShape.of_string(t)) {
    | InvalidText(_) =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (ctx, InvalidText(u, t), id_gen);
    | Int
    | Bool
    | Float =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (ctx, TyVar(InHole(BuiltinType, u), t), id_gen);
    | ExpandingKeyword(_) =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (ctx, TyVar(InHole(ReservedKeyword, u), t), id_gen);
    | TyVar(_) =>
      if (TyVar.is_valid(t)) {
        let ctx = Context.add_tyvar(ctx, t, k);
        (ctx, TyVar(NotInHole, t), id_gen);
      } else {
        let (u, id_gen) = IDGen.next_hole(id_gen);
        (ctx, InvalidText(u, t), id_gen);
      }
    }
  };

let ana_fix_holes_z =
    (ctx: Context.t, ztp: ZTPat.t, k: Kind.t, id_gen: IDGen.t)
    : (Context.t, ZTPat.t, IDGen.t) => {
  let path = CursorPath_TPat.of_z(ztp);
  let (ctx, new_tp, id_gen) =
    ana_fix_holes(ctx, ZTPat.erase(ztp), k, id_gen);
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
