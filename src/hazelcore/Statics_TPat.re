let matches = (ctx: Contexts.t, t: TPat.t, _ty: HTyp.t): Contexts.t => {
  switch (t) {
  | EmptyHole => ctx
  | TyVar(NotInHole, _name)
  // => Contexts.bind_tyvar(ctx, name, k)
  | TyVar(InHole(_), _name) => ctx
  };
};

let fix_holes =
    (ctx: Contexts.t, p: TPat.t, u_gen: MetaVarGen.t)
    : (Contexts.t, TPat.t, MetaVarGen.t) => {
  switch (p) {
  | EmptyHole => (ctx, EmptyHole, u_gen)
  | TyVar(_, t) =>
    let tp = TPat.of_string(t);
    switch (tp) {
    | EmptyHole => (ctx, EmptyHole, u_gen)
    | TyVar(NotInHole, _name) as t =>
      // let ctx = Contexts.bind_tyvar(ctx, name, k);
      (ctx, t, u_gen)
    | TyVar(InHole(_), _) as t => (ctx, t, u_gen)
    };
  };
};

let fix_holes_z =
    (ctx: Contexts.t, zp: ZTPat.t, u_gen: MetaVarGen.t)
    : (Contexts.t, ZTPat.t, MetaVarGen.t) => {
  let path = CursorPath_TPat.of_z(zp);
  let (ctx, new_p, u_gen) = fix_holes(ctx, ZTPat.erase(zp), u_gen);
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
