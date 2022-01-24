// [@deriving sexp]
type t = VarMap.t_(HTyp.t);
include (module type of VarMap);
