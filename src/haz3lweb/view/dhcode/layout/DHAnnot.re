open Sexplib.Std;
open Haz3lcore;

[@deriving sexp]
type t =
  | Collapsed
  | Step(int)
  | Term
  | HoleLabel
  | Delim
  | EmptyHole(bool, HoleInstance.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, HoleInstance.t)
  | VarHole(VarErrStatus.HoleReason.t, HoleInstance.t)
  | InconsistentBranches(HoleInstance.t)
  | Invalid(HoleInstance.t)
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration
  | OperationError(InvalidOperationError.t)
  | Steppable(list(UpdateAction.stepper_action))
  | Stepped
  | Substituted;
