[@deriving sexp]
type t = int;
let eq: (t, t) => bool;
let compare: (t, t) => int;
let join: (t, t) => t;
