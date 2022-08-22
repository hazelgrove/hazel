include (module type of VarMap);

[@deriving sexp]
type t = VarMap.t(HTyp.t);
