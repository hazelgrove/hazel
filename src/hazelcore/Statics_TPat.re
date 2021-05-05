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
