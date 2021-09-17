[@deriving sexp]
type t =
  | Collapsed
  | HoleLabel
  | Delim
  | EmptyHole(bool, HoleInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, HoleInstance.t)
  | InjHole(InjErrStatus.HoleReason.t, HoleInstance.t)
  | EmptyTagHole(bool, MetaVar.t)
  | NonEmptyTagHole(TagErrStatus.HoleReason.t, MetaVar.t)
  | InconsistentBranches(HoleInstance.t)
  | Invalid(HoleInstance.t)
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration
  | DivideByZero;
