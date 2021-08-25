open Pretty;

let promote_annot =
  fun
  | HTypAnnot.HoleLabel => DHAnnot.HoleLabel
  | HTypAnnot.Delim => DHAnnot.Delim;

let promote = (d: HTypDoc.t): DHDoc.t => d |> Doc.map_annot(promote_annot);

let mk = (~enforce_inline as _: bool, tag: UHTag.t): DHDoc.t =>
  tag |> HTypDoc_Tag.mk |> promote;
