include (module type of VarMap);

type nonrec t = t(DHExp.t);

let sexp_of_t: t => Sexplib.Sexp.t;
let t_of_sexp: Sexplib.Sexp.t => t;

let id_env: Context.t => t;
