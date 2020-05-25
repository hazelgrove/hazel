open Sexplib.Std;

[@deriving sexp]
type t =
  | Collapsed
  | HoleLabel
  | Delim
  | EmptyHole(bool, HoleInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleInstance.t)
  | ExpVarHole(ExpVarErrStatus.HoleReason.t, HoleInstance.t)
  | PatVarHole(PatVarErrStatus.HoleReason.t, HoleInstance.t)
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration;
