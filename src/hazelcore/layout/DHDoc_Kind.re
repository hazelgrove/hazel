open Pretty.Doc;

let mk = (k: Kind.t, ~enforce_inline: bool): DHDoc.t => {
  switch (k) {
  | Kind.Type => text("Ty")
  | Kind.Singleton(ty) =>
    hcats([
      text("S("),
      DHDoc_Typ.mk(~enforce_inline, HTyp.of_unsafe(ty)),
      text(")"),
    ])
  | Kind.KHole => text("KHole")
  };
};
