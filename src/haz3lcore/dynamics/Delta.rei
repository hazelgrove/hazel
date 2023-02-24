[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type val_ty = (hole_sort, Typ.t, Ctx.t);

[@deriving sexp]
type t = MetaVarMap.t((hole_sort, Typ.t, Ctx.t));

let empty: t;

let add: (MetaVar.t, (hole_sort, Typ.t, Ctx.t), t) => t;

let union: (t, t) => t;

let find_opt: (int, t) => option(val_ty);

let cmp: (val_ty, val_ty) => bool;

let equal: ((val_ty, val_ty) => bool, t, t) => bool;
