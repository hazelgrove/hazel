[@deriving sexp]
type t =
  | Comma
  | Space
  | Cons;

let to_string: t => string;

let to_parse_string: t => string;

let is_Space: t => bool;
