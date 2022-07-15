[@deriving sexp]
type t;

let of_string: string => t;
let to_string: t => string;

let apply: (string => string, t) => t;

let empty: t;
let length: t => int;
