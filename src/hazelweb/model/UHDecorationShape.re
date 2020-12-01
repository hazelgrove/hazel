[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | FilledHole(UHExp.t, Synthesizing.filled_holes)
  | FilledHoleZ(UHExp.t, Synthesizing.filled_holes, Synthesizing.t)
  | FillingHole(ZList.t(UHExp.t, UHExp.t), Shmyth.constraint_data);
