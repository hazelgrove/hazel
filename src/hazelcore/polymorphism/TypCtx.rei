[@deriving sexp]
type t = VarSet.t_;
include (module type of VarSet);
