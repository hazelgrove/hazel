[@deriving sexp]
type t =
  | Arrow
  | Prod
  | Sum
  | Space;

let to_string =
  fun
  | Arrow => Unicode.typeArrowSym
  | Prod => ","
  | Sum => "|"
  | Space => " ";

let precedence =
  fun
  | Sum => 3
  | Arrow => 2
  | Prod => 1
  | Space => 4;

let precedence_const = 4;

let associativity =
  fun
  | Arrow => Associativity.Right
  | _ => Associativity.Left;
