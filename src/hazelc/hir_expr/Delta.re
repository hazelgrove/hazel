open Holes;

[@deriving sexp]
type hole_sort =
  | ExpressionHole
  | PatternHole;

[@deriving sexp]
type t = MetaVarMap.t((hole_sort, Typ.t, Ident.Map.t(Typ.t)));

let empty = MetaVarMap.empty;
