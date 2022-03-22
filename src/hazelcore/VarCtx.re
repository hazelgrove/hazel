[@deriving sexp]
type t = VarMap.t_(HTyp.t);
include VarMap;

let subst_tyvar = (gamma: t, i: Index.t, ty: HTyp.t): t =>
  VarMap.map(((_, ty_x)) => HTyp.subst(ty_x, i, ty), gamma);
