module Typ = {
  [@deriving sexp]
  type t =
    | Arrow
    | Prod
    | Sum;

  let string_of_operator =
    fun
    | Arrow => UnicodeConstants.typeArrowSym
    | Prod => ","
    | Sum => "|";

  let parse_string_of_operator = op =>
    switch (op) {
    | Arrow => "->"
    | _ => string_of_operator(op)
    };
};

module Pat = {
  [@deriving sexp]
  type t =
    | Comma
    | Space
    | Cons;

  let string_of_operator =
    fun
    | Comma => ","
    | Space => " "
    | Cons => "::";

  let parse_string_of_operator = op =>
    switch (op) {
    | Space => "_"
    | _ => string_of_operator(op)
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

  let string_of_operator =
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

  let parse_string_of_operator = op =>
    switch (op) {
    | Equals => "="
    | Space => "_"
    | And => "&"
    | Or => "|"
    | _ => string_of_operator(op)
    };

  let is_Space =
    fun
    | Space => true
    | _ => false;
};
