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
  | Sum => 1
  | Arrow => 2
  | Prod => 3;

let precedence_const = 0;

let associativity =
  fun
  | Arrow => Associativity.Right
  | _ => Associativity.Left;
