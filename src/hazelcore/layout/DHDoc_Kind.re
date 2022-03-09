open Pretty.Doc;

let rec mk = (k: Kind.t, ~enforce_inline: bool): DHDoc.t => {
  switch (k) {
  | Kind.Type => text("Ty")
  | Kind.Singleton(ty) =>
    hcats([
      text("S("),
      mk(~enforce_inline, Type),
      text(","),
      DHDoc_Typ.mk(~enforce_inline, HTyp.of_unsafe(ty)),
      text(")"),
    ])
  | Kind.KHole => text("KHole")
  };
};
