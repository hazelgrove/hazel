[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | CellBoundary;
