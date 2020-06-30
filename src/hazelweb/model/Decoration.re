[@deriving sexp]
type t =
  | ErrHole
  | CurrentTerm(CursorPosition.t);
