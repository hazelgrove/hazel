[@deriving sexp]
type t =
  | Arrow
  | Prod
  | Sum;

let to_string =
  fun
  | Arrow => UnicodeConstants.typeArrowSym
  | Prod => ","
  | Sum => "|";

let to_parse_string = op =>
  switch (op) {
  | Arrow => "->"
  | _ => to_string(op)
  };

let precedence =
  fun
  | Prod => 1
  | Arrow => 2
  | Sum => 3;

let precedence_const = 4;

let associativity =
  fun
  | Arrow => Associativity.Right
  | _ => Associativity.Left;
