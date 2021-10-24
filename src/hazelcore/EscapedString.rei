[@deriving sexp]
type t = string;

let to_string: t => string;

let equal: (t, t) => bool;

let concat: (t, t) => t;
