type t = Pretty.Doc.t(KindAnnot.t);

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, Kind.t) => t;
