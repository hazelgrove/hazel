[@deriving sexp]
type t =
  | Comma
  | Space
  | Cons;

let to_string =
  fun
  | Comma => ","
  | Space => " "
  | Cons => "::";

let to_parse_string = op =>
  switch (op) {
  | Space => "_"
  | _ => to_string(op)
  };

let is_Space =
  fun
  | Space => true
  | _ => false;

let precedence =
  fun
  | Comma => 0
  | Cons => 1
  | Space => 2;

let associativity =
  fun
  | Comma => Associativity.Right
  | _ => Associativity.Left;
