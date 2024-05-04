[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | OutOfFuel
  | StepDoesNotMatch
  | FreeInvalidVar(Var.t)
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t)
  | InvalidBoxedTypFun(DHExp.t)
  | InvalidBoxedFun(DHExp.t)
  | InvalidBoxedModule(DHExp.t)
  | InvalidBoxedBoolLit(DHExp.t)
  | InvalidBoxedIntLit(DHExp.t)
  | InvalidBoxedFloatLit(DHExp.t)
  | InvalidBoxedListLit(DHExp.t)
  | InvalidBoxedStringLit(DHExp.t)
  | InvalidBoxedTuple(DHExp.t)
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DHExp.t))
  | InvalidProjection(int);

[@deriving (show({with_path: false}), sexp, yojson)]
exception Exception(t);
