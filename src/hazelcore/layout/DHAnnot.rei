[@deriving sexp]
type t =
  | Collapsed
  | Step(int)
  | Term
  | HoleLabel
  | Delim
  | EmptyHole(bool, HoleClosure.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleClosure.t)
  | VarHole(VarErrStatus.HoleReason.t, HoleClosure.t)
  | InconsistentBranches(HoleClosure.t)
  | Invalid(HoleClosure.t)
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration
  | DivideByZero;
