[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type val_ty = (hole_sort, Typ.t, Ctx.t);

[@deriving sexp]
type t = MetaVarMap.t(val_ty);

let empty: t = (MetaVarMap.empty: t);

let add = MetaVarMap.add;

let union = MetaVarMap.disj_union;

let cmp = (a: val_ty, b: val_ty): bool => {
  a == b;
};

let equal = MetaVarMap.equal;
