[@deriving sexp]
type t = VarMap.t_(DHExp.t);
include (module type of VarMap);
