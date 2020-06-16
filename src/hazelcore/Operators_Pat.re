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
