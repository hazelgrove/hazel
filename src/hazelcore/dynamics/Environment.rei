include (module type of VarMap);

[@deriving sexp]
type nonrec t = t(DHExp.t);

let id_env: Context.t => t;
