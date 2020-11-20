[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | LivelitExpression(Livelits.LivelitView.shape);
