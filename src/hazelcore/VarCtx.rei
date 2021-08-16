[@deriving (sexp, show)]
type t = VarMap.t_(HTyp.t);
include (module type of VarMap);
