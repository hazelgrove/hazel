[@deriving sexp]
type t =
  | Arrow
  | Prod;

let to_string =
  fun
  | Arrow => Unicode.typeArrowSym
  | Prod => ",";

let precedence =
  fun
  | Arrow => 2
  | Prod => 1;

let precedence_const = 4;

let associativity =
  fun
  | Arrow => Associativity.Right
  | Prod => Associativity.Left;
