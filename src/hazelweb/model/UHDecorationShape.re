[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | FilledHole(UHExp.t, Synthesizing.t)
  | FillingHole(ZList.t(UHExp.t, UHExp.t));
