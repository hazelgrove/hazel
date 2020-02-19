[@deriving sexp]
type t =
  | HoleLabel
  | Delim
  | EmptyHole(HoleInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, HoleInstance.t)
  | FailedCastDecoration
  | CastDecoration;
