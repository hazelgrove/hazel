[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type t = MetaVarMap.t((hole_sort, Typ.t, Ctx.t));

let empty: t = (MetaVarMap.empty: t);

let add = MetaVarMap.add;

let union = MetaVarMap.disj_union;
