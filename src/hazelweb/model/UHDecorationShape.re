[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | FilledHole(Synthesizing.t, Synthesizing.filled_holes)
  | FillingHole(ZList.t(UHExp.t, UHExp.t));
