open Pretty.Doc;

let mk = (p: TPat.t, ~enforce_inline: bool): DHDoc.t => {
  let _ = enforce_inline;
  switch (p) {
  | EmptyHole => annot(DHAnnot.Delim, annot(DHAnnot.HoleLabel, text("?")))
  | TyVar(NotInHole, name) => text(name)
  | InvalidText(_, name)
  | TyVar(InHole(_), name) => annot(DHAnnot.TyVarHole, text(name))
  };
};
