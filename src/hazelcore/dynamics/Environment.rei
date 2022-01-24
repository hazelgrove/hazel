// [@deriving sexp]
type t = VarMap.t_(DHExp.t);
include (module type of VarMap);

let id_env: VarCtx.t => t;
