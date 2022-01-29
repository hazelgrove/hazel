open Pretty.Doc;

let mk = (p: TPat.t, ~enforce_inline: bool): DHDoc.t => {
  let _ = enforce_inline;
  switch (p) {
  | TPat.EmptyHole =>
    annot(DHAnnot.Delim, annot(DHAnnot.HoleLabel, text("?")))
  | TPat.TyVar(NotInHole, name) => text(TyVar.Name.to_string(name))
  | TPat.TyVar(InHole(_reason, _u), name) =>
    annot(DHAnnot.TyVarHole, text(TyVar.Name.to_string(name)))
  };
};
