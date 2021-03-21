type hole_sort =
  | ExpressionHole
  | TypeHole
  | PatternHole;

type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));

let empty: t;

let union: (t, t) => t;
