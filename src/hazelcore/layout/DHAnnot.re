open Sexplib.Std;

[@deriving sexp]
type t =
  | Collapsed
  | HoleLabel
  | Delim
  | EmptyHole(bool, NodeInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, NodeInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, NodeInstance.t)
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration;
