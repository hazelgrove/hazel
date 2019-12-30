[@deriving sexp]
type t = VarMap.t_((HTyp.t, CursorPath.steps));
include VarMap;
