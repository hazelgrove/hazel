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
