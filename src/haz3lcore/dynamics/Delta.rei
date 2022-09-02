type hole_sort =
  | ExpressionHole
  | PatternHole;

type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));

let empty: t;
