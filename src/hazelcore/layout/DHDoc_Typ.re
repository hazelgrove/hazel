open Pretty;

type t = Doc.t(DHAnnot.t);

module Typ = {
  let promote_annot =
    fun
    | HTypAnnot.HoleLabel => DHAnnot.HoleLabel
    | HTypAnnot.Delim => DHAnnot.Delim;
  let promote = (d: HTypDoc.t): t => d |> Doc.map_annot(promote_annot);
  let mk = (~enforce_inline: bool, ty: HTyp.t): t =>
    ty |> HTypDoc.mk(~enforce_inline) |> promote;
};
