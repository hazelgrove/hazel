[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type val_ty = (hole_sort, Typ.t(IdTag.t), Ctx.t(IdTag.t));

[@deriving sexp]
type t = Id.Map.t(val_ty);

let empty: t;
