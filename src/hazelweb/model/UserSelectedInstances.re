[@deriving sexp]
type t = MetaVarMap.t(HoleClosureId.t);

let init = MetaVarMap.empty;
let find_opt = MetaVarMap.find_opt;
let add = MetaVarMap.add;
