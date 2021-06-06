[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | TyVarUse
  | CurrentTerm;
