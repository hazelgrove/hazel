[@deriving (sexp, show)]
type t = MetaVarMap.t(MetaVarInst.t);

let init = MetaVarMap.empty;
let find_opt = MetaVarMap.find_opt;
let add = MetaVarMap.add;
