open TPat;

let matches = (ctx: Contexts.t, t: TPat.t, _ty: HTyp.t, k: Kind.t): Contexts.t => {
  switch (t) {
  | EmptyHole => ctx
  | TyVar(None, id) => Contexts.extend_tyvars(ctx, (id, k))
  | TyVar(Some(Keyword(_kw)), _id) => ctx
  };
};
