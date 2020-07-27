[@deriving sexp]
type t =
  | UnaryMinus
  | FUnaryMinus;

let to_string =
  fun
  | UnaryMinus => "-"
  | FUnaryMinus => "-.";

let to_parse_string = to_string;

// let is_Space = _ => false;
