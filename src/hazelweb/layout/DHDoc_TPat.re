open Pretty.Doc;

let mk(p: TPat.t, ~enforce_inline: bool): DHDoc.t = {
  let _ = enforce_inline;
  switch (p) {
  | TPat.EmptyHole => annot(DHAnnot.Delim, annot(DHAnnot.HoleLabel, text("?")))
  | TPat.TyVar(None, tyid) => text(TyId.to_string(tyid))
  | TPat.TyVar(Some(_kw), tyid) =>
      annot(DHAnnot.TyVarHole, text(TyId.to_string(tyid)))
  }
}
