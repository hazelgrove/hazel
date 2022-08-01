[@deriving sexp]
type t;

let v: string => t;
let of_string: string => t;
let to_string: t => string;

let equal: (t, t) => bool;
let compare: (t, t) => int;
let length: t => int;

let concat: (t, t) => t;
let join: (t, t) => t;

module Map: {
  [@deriving sexp]
  type binding('v) = (t, 'v);

  include Map.S with type key = t;

  let sexp_of_t: ('v => Sexplib.Sexp.t, t('v)) => Sexplib.Sexp.t;
  let t_of_sexp: (Sexplib.Sexp.t => 'v, Sexplib.Sexp.t) => t('v);
};
