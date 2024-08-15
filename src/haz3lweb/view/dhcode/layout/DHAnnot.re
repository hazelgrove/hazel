open Util;
open Haz3lcore;

[@deriving sexp]
type t =
  | Collapsed
  | Step(int)
  | Term
  | HoleLabel
  | Delim
  | EmptyHole(bool, ClosureEnvironment.t)
  | NonEmptyHole
  | VarHole(VarErrStatus.HoleReason.t, Id.t)
  | InconsistentBranches(Id.t)
  | Invalid
  | FailedCastDelim
  | FailedCastDecoration
  | CastDecoration
  | OperationError(InvalidOperationError.t)
  | DerivationError(DerivationError.t)
  | Steppable(int)
  | Stepped
  | Substituted;
