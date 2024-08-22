open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | OutOfFuel
  | StepDoesNotMatch
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t(IdTag.t))
  | InvalidBoxedTypFun(DHExp.t(IdTag.t))
  | InvalidBoxedFun(DHExp.t(IdTag.t))
  | InvalidBoxedBoolLit(DHExp.t(IdTag.t))
  | InvalidBoxedIntLit(DHExp.t(IdTag.t))
  | InvalidBoxedFloatLit(DHExp.t(IdTag.t))
  | InvalidBoxedListLit(DHExp.t(IdTag.t))
  | InvalidBoxedStringLit(DHExp.t(IdTag.t))
  | InvalidBoxedSumConstructor(DHExp.t(IdTag.t))
  | InvalidBoxedTuple(DHExp.t(IdTag.t))
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DHExp.t(IdTag.t)))
  | InvalidProjection(int);

exception Exception(t);
