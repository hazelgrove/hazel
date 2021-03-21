type hole_sort =
  | ExpressionHole
  | TypeHole
  | PatternHole;

type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));
let empty: t = (MetaVarMap.empty: t);

let union = (d1, d2) => MetaVarMap.union((_, a, _) => Some(a), d1, d2);
