open Pretty.Doc;

let mk = (k: Kind.t, ~enforce_inline: bool): DHDoc.t => {
  switch (k) {
  | Hole => text("Hole")
  | Type => text("Type")
  | S(ty) =>
    hcats([
      text("S("),
      DHDoc_Typ.mk(~enforce_inline, HTyp.of_syntax(ty)),
      text(")"),
    ])
  };
};
