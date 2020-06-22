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
  | Or;

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

let precedence =
  fun
  | Plus => 1
  | Minus => 1
  | Times => 2
  | Divide => 2
  | FPlus => 1
  | FMinus => 1
  | FTimes => 2
  | FDivide => 2
  | LessThan => 5
  | GreaterThan => 5
  | Equals => 5
  | FLessThan => 5
  | FGreaterThan => 5
  | FEquals => 5
  | Comma => 0
  | Cons => 0
  | And => 3
  | Or => 3
  | Space => 0;
