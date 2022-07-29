[@deriving sexp]
type t;

let of_int: t => int;
let to_int: t => int;

let init: t;
let next: t => t;
