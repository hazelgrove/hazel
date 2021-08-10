open Pretty;

let promote_annot =
  fun
  | HTypAnnot.HoleLabel => DHAnnot.HoleLabel
  | HTypAnnot.Delim => DHAnnot.Delim;

let promote = (d: HTypDoc.t): DHDoc.t => d |> Doc.map_annot(promote_annot);

let mk = (tag: UHTag.t): DHDoc.t => tag |> HTagDoc.mk |> promote;
