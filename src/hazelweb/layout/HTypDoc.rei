type t = Pretty.Doc.t(HTypAnnot.t);

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, HTyp.t) => t;

// TODO: make + work inside tag arg bodies

// TODO: swap left / right
// TODO: tag hole tabbing
// TODO: clean up doc files for tags
