[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type t = MetaVarMap.t((hole_sort, Typ.t, Ctx.t));

let empty: t;

let add: (MetaVar.t, (hole_sort, Typ.t, Ctx.t), t) => t;

let union: (t, t) => t;
