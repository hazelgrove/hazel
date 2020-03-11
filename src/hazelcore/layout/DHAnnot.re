[@deriving sexp]
type t =
  | Collapsed
  | HoleLabel
  | Delim
  | EmptyHole(HoleInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, HoleInstance.t)
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration;
