module Typ = {
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
};

module Pat = {
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
};

module Exp = {
  [@deriving sexp]
  type t =
    | Space
    | Plus
    | Minus
    | Times
    | LessThan
    | GreaterThan
    | Equals
    | Comma
    | Cons
    | And
    | Or;

  let to_string =
    fun
    | Space => " "
    | Plus => "+"
    | Minus => "-"
    | Times => "*"
    | LessThan => "<"
    | GreaterThan => ">"
    | Equals => "=="
    | Comma => ","
    | Cons => "::"
    | And => "&&"
    | Or => "||";

  let to_parse_string = op =>
    switch (op) {
    | Equals => "="
    | Space => "_"
    | And => "&"
    | Or => "|"
    | _ => to_string(op)
    };

  let is_Space =
    fun
    | Space => true
    | _ => false;
};
