open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | OutOfFuel
  | StepDoesNotMatch
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t)
  | InvalidBoxedTypFun(DHExp.t)
  | InvalidBoxedFun(DHExp.t)
  | InvalidBoxedBoolLit(DHExp.t)
  | InvalidBoxedIntLit(DHExp.t)
  | InvalidBoxedFloatLit(DHExp.t)
  | InvalidBoxedListLit(DHExp.t)
  | InvalidBoxedStringLit(DHExp.t)
  | InvalidBoxedLabel(DHExp.t)
  | InvalidBoxedSumConstructor(DHExp.t)
  | InvalidBoxedTupLabel(DHExp.t)
  | InvalidBoxedTuple(DHExp.t)
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DHExp.t))
  | InvalidProjection(int)
  | IntegerTooBig(Bigint.t);

exception Exception(t);
