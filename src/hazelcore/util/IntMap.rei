include (module type of Map.Make(Int));

[@deriving sexp]
type binding('v) = (int, 'v);

let sexp_of_t: ('v => Sexplib.Sexp.t, t('v)) => Sexplib.Sexp.t;

let t_of_sexp: (Sexplib.Sexp.t => 'v, Sexplib.Sexp.t) => t('v);
