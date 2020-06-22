[@deriving sexp]
type t =
  | KHole
  | Type;

let consistent: (t, t) => bool;

let to_string: t => string;
