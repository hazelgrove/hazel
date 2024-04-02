[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type t = Id.Map.t((hole_sort, Typ.t, VarCtx.t));

let empty: t;
