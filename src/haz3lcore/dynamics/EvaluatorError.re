open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | OutOfFuel
  | StepDoesNotMatch
  | FreeInvalidVar(Var.t)
  | BadPatternMatch
  | CastBVHoleGround(DExp.t)
  | InvalidBoxedFun(DExp.t)
  | InvalidBoxedBoolLit(DExp.t)
  | InvalidBoxedIntLit(DExp.t)
  | InvalidBoxedFloatLit(DExp.t)
  | InvalidBoxedListLit(DExp.t)
  | InvalidBoxedStringLit(DExp.t)
  | InvalidBoxedTuple(DExp.t)
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DExp.t))
  | InvalidProjection(int);

exception Exception(t);
