[@deriving sexp]
type t =
  | Negate
  | FNegate;

let to_string =
  fun
  | Negate => "-"
  | FNegate => "-.";

let to_parse_string = to_string;
