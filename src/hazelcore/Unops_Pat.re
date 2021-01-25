[@deriving sexp]
type t =
  | Negate
  | FNegate;

let to_string =
  fun
  | Negate => "-"
  | FNegate => "-.";

let is_Space =
  fun
  | _ => false;
