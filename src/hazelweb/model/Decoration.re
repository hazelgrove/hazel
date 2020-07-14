[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | CurrentTerm(TermSort.t, TermShape.t);
