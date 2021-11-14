type sort_of_hole =
  | ExpressionHole
  | PatternHole;

type t = MetaVarMap.t((sort_of_hole, HTyp.t, VarCtx.t));

let empty: t;
