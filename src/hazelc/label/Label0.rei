[@deriving (sexp, eq, ord)]
type t;

let of_int: int => t;
let to_int: t => int;

let init: t;
let next: t => t;

let max: (t, t) => t;
