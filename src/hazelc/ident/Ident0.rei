[@deriving sexp]
type t;

let equal: (t, t) => bool;
let compare: (t, t) => int;
let length: t => int;

let v: string => t;
let to_string: t => string;
let of_string: string => t;
let map: (string => string, t) => t;

let concat: (t, t) => t;
let join: (t, t) => t;
