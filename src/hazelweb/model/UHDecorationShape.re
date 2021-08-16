[@deriving (sexp, show)]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm;
