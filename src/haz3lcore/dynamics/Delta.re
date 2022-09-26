[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));
let empty: t = (MetaVarMap.empty: t);
