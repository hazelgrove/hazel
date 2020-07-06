[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | CurrentTerm(TermShape.t);
