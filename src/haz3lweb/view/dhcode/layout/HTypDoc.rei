open Haz3lcore;

type t = Pretty.Doc.t(HTypAnnot.t);

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, Typ.t(IdTag.t)) => t;
