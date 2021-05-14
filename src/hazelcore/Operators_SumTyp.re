[@deriving sexp]
type t =
  | Plus;

let to_string =
  fun
  | Plus => "+";

let precedence =
  fun
  | Plus => 3;

let associativity =
  fun
  | Plus => Associativity.Left;
