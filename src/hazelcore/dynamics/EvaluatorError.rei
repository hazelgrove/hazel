[@deriving sexp]
type t =
  | FreeInvalidVar(Var.t)
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t)
  | InvalidBoxedFun(DHExp.t)
  | InvalidBoxedBoolLit(DHExp.t)
  | InvalidBoxedIntLit(DHExp.t)
  | InvalidBoxedFloatLit(DHExp.t)
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DHExp.t));

[@deriving sexp]
exception Exception(t);
