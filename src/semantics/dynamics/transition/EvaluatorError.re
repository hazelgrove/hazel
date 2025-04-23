open Util;

/* EvaluatorErrors should never be thrown, they indicate something wrong
   with Hazel's implementation. (As opposed to InvalidOperationErrors
   which indicate something wrong with the user's code.) */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InvalidBoxedTypFun(DHExp.t)
  | InvalidBoxedFun(DHExp.t)
  | InvalidBoxedBoolLit(DHExp.t)
  | InvalidBoxedIntLit(DHExp.t)
  | InvalidBoxedFloatLit(DHExp.t)
  | InvalidBoxedListLit(DHExp.t)
  | InvalidBoxedListCons(DHExp.t)
  | InvalidBoxedStringLit(DHExp.t)
  | InvalidBoxedNatLit(DHExp.t)
  | InvalidBoxedLabel(DHExp.t)
  | InvalidBoxedSumConstructor(DHExp.t)
  | InvalidBoxedTupLabel(DHExp.t)
  | InvalidBoxedTuple(DHExp.t)
  | InvalidBuiltin(string);

exception Exception(t);
