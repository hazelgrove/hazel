[@deriving sexp]
type t = VarMap.t_(HTyp.t);
include (module type of VarMap);

let subst_tyvar: (t, Index.t(Index.abs), HTyp.t) => t;
