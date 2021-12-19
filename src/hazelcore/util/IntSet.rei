include (module type of Set.Make(Int));

let sexp_of_t: t => Sexplib.Sexp.t;

let t_of_sexp: Sexplib.Sexp.t => t;
