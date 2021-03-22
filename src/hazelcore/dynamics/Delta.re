type hole_sort(_, _) =
  | ExpressionHole: hole_sort(HTyp.t, VarCtx.t)
  | TypeHole: hole_sort(Kind.t, TyVarCtx.t)
  | PatternHole: hole_sort(HTyp.t, VarCtx.t);

type value =
  | V(hole_sort('s, 'ctx), 's, 'ctx): value;

include MetaVarMap;
type t = MetaVarMap.t(value);

let empty: t = (MetaVarMap.empty: t);

let union = (d1, d2) => MetaVarMap.union((_, a, _) => Some(a), d1, d2);
