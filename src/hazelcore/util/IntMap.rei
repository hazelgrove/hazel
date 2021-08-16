include (module type of Map.Make(Int));
module Sexp = Sexplib.Sexp;

[@deriving (sexp, show)]
type binding('v) = (int, 'v);

let sexp_of_t: ('v => Sexp.t, t('v)) => Sexp.t;
let t_of_sexp: (Sexp.t => 'v, Sexp.t) => t('v);

let pp: (Format.formatter, t('a)) => unit;
