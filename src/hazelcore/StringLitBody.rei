[@deriving sexp]
type t = pri string;
let of_string: string => t;
let to_string: t => string;
