open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | OutOfFuel
  | StepDoesNotMatch
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t(list(Id.t)))
  | InvalidBoxedTypFun(DHExp.t(list(Id.t)))
  | InvalidBoxedFun(DHExp.t(list(Id.t)))
  | InvalidBoxedBoolLit(DHExp.t(list(Id.t)))
  | InvalidBoxedIntLit(DHExp.t(list(Id.t)))
  | InvalidBoxedFloatLit(DHExp.t(list(Id.t)))
  | InvalidBoxedListLit(DHExp.t(list(Id.t)))
  | InvalidBoxedStringLit(DHExp.t(list(Id.t)))
  | InvalidBoxedSumConstructor(DHExp.t(list(Id.t)))
  | InvalidBoxedTuple(DHExp.t(list(Id.t)))
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DHExp.t(list(Id.t))))
  | InvalidProjection(int);

exception Exception(t);
