[@deriving sexp]
type t =
  | IMinus
  | FMinus;

let to_string =
  fun
  | IMinus => "-"
  | FMinus => "-.";

let to_parse_string = to_string;

// let is_Space = _ => false;
