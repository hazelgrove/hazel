type hole_sort(_, _) =
  | ExpressionHole: hole_sort(HTyp.t, VarCtx.t)
  | TypeHole: hole_sort(Kind.t, TyVarCtx.t)
  | PatternHole: hole_sort(HTyp.t, VarCtx.t);

type value =
  | V(hole_sort('s, 'ctx), 's, 'ctx): value;

type t = MetaVarMap.t(value);

let empty: t;

let union: (t, t) => t;

let add: (int, value, t) => t;
