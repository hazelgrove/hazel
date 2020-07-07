open Pretty;

let promote_annot =
  fun
  | HTypAnnot.HoleLabel => DHAnnot.HoleLabel
  | HTypAnnot.Delim => DHAnnot.Delim
  | HTypAnnot.TyVarHole => DHAnnot.TyVarHole;
let promote = (d: HTypDoc.t): DHDoc_common.t =>
  d |> Doc.map_annot(promote_annot);
let mk = (~enforce_inline: bool, ty: HTyp.t): DHDoc_common.t =>
  ty |> HTypDoc.mk(~enforce_inline) |> promote;
