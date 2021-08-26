type t = Pretty.Doc.t(HTypAnnot.t);

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, HTyp.t) => t;

// TODO: tag hole tabbing

// TODO: add a status to tag holes (later)
//   - invalid tag name (e.g., bad chars, no initial majuscule)
//   - duplicate tag name
