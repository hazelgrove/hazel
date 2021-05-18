open TPat;

let matches = (ctx: Contexts.t, t: TPat.t, _ty: HTyp.t, k: Kind.t): Contexts.t => {
  switch (t) {
  | EmptyHole => ctx
  | TyVar(None, id) => Contexts.extend_tyvars(ctx, (id, k))
  | TyVar(Some(Keyword(_kw)), _id) => ctx
  };
};

let fix_holes = (ctx: Contexts.t, t: TPat.t, k: Kind.t): (Contexts.t, TPat.t) => {
  switch (t) {
  | EmptyHole => (ctx, EmptyHole)
  | TyVar(_, id) =>
    switch (ExpandingKeyword.mk(TyId.to_string(id))) {
    | None => (Contexts.extend_tyvars(ctx, (id, k)), TyVar(None, id))
    | Some(kw) => (ctx, TyVar(Some(Keyword(kw)), id))
    }
  };
};

let fix_holes_z =
    (ctx: Contexts.t, zp: ZTPat.t, k: Kind.t): (Contexts.t, ZTPat.t) => {
  let path = CursorPath_TPat.of_z(zp);
  let (ctx, new_p) = fix_holes(ctx, ZTPat.erase(zp), k);
  let zp =
    CursorPath_TPat.follow(path, new_p)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ctx, zp);
};
