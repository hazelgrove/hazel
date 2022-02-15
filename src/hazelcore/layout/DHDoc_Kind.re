open Pretty.Doc;

let rec mk = (k: Kind.t, ~enforce_inline: bool): DHDoc.t => {
  switch (k) {
  | Kind.Type => text("Ty")
  | Kind.Singleton(k', ty) =>
    hcats([
      text("S("),
      mk(~enforce_inline, k'),
      text(","),
      DHDoc_Typ.mk(~enforce_inline, ty),
      text(")"),
    ])
  | Kind.KHole => text("KHole")
  };
};
