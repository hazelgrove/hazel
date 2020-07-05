[@deriving sexp]
type t =
  | ErrHole
  | CurrentTerm(TermShape.t);
