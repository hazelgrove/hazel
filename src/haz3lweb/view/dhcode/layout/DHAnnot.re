open Sexplib.Std;
open Haz3lcore;

[@deriving sexp]
type t =
  | Collapsed
  | Step(int)
  | Term
  | HoleLabel
  | Delim
  | EmptyHole(bool, ClosureEnvironment.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, HoleInstance.t)
  | InconsistentBranches(HoleInstance.t)
  | Invalid
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration
  | OperationError(InvalidOperationError.t)
  | Steppable(int)
  | Stepped
  | Substituted;
