[@deriving sexp]
type t =
  | Space
  | Plus
  | Minus
  | Times
  | Divide
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | LessThan
  | GreaterThan
  | Equals
  | FLessThan
  | FGreaterThan
  | FEquals
  | Comma
  | Cons
  | And
  | Or
  | Caret;

let to_string =
  fun
  | Space => " "
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Divide => "/"
  | FPlus => "+."
  | FMinus => "-."
  | FTimes => "*."
  | FDivide => "/."
  | LessThan => "<"
  | GreaterThan => ">"
  | Equals => "=="
  | FLessThan => "<."
  | FGreaterThan => ">."
  | FEquals => "==."
  | Comma => ","
  | Cons => "::"
  | And => "&&"
  | Or => "||"
  | Caret => "^";

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
