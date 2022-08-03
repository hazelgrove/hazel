[@deriving sexp]
type t;

let of_int: int => t;
let to_int: t => int;

let init: t;
let next: t => t;

let compare: (t, t) => int;
let equal: (t, t) => bool;

let max: (t, t) => t;
