open Pretty.Doc;

let mk = (tp: TPat.t, ~enforce_inline: bool): DHDoc.t => {
  let _ = enforce_inline;
  switch (tp) {
  | EmptyHole => annot(DHAnnot.Delim, annot(DHAnnot.HoleLabel, text("?")))
  | TyVar(NotInHole, t) => text(t)
  | InvalidText(_, t)
  | TyVar(InHole(_), t) => annot(DHAnnot.TyVarHole, text(t))
  };
};
