[@deriving sexp]
type t = pri string;
let of_string: string => t;
let to_string: t => string;

let to_unescaped_string: t => UnescapedString.t;
