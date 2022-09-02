open Haz3lcore;
open Pretty;

let promote_annot =
  fun
  | HTypAnnot.Term => DHAnnot.Term
  | HTypAnnot.Step(n) => DHAnnot.Step(n)
  | HTypAnnot.HoleLabel => DHAnnot.HoleLabel
  | HTypAnnot.Delim => DHAnnot.Delim;
let promote = (d: HTypDoc.t): DHDoc.t => d |> Doc.map_annot(promote_annot);
let mk = (~enforce_inline: bool, ty: HTyp.t): DHDoc.t =>
  ty |> HTypDoc.mk(~enforce_inline) |> promote;
