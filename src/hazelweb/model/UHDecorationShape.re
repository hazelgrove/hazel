[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | ExplanationElems
  | CurrentTerm;
