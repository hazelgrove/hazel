[@deriving sexp]
type t =
  | Arrow
  | Prod
  | Sum;

let to_string =
  fun
  | Arrow => Unicode.typeArrowSym
  | Prod => ","
  | Sum => "|";

let precedence =
  fun
  | Sum => 3
  | Arrow => 2
  | Prod => 1;

let precedence_const = 4;

let associativity =
  fun
  | Arrow => Associativity.Right
  | _ => Associativity.Left;
