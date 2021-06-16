[@deriving sexp]
type t =
  | Collapsed
  | HoleLabel
  | Delim
  | FreeLivelitLabel
  | EmptyHole(bool, NodeInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, NodeInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, NodeInstance.t)
  | InconsistentBranches(NodeInstance.t)
  | FreeLivelit(LivelitName.t, NodeInstance.t)
  | Invalid(NodeInstance.t)
  | FailedCastDelim
  | FailedCastDecoration
  | InvalidOpDecoration
  | CastDecoration
  | DivideByZero
  | String;
