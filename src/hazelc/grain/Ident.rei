[@deriving sexp]
type t;

let v: string => t;
let to_string: t => string;
let of_string: string => t;
let map: (string => string, t) => t;

let concat: (t, t) => t;
let join: (t, t) => t;
