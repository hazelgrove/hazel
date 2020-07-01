[@deriving sexp]
type t =
  | Minus
  | FMinus;

let to_string =
  fun
  | Minus => "-"
  | FMinus => "-.";

let to_parse_string = to_string;

// let is_Space = _ => false;
