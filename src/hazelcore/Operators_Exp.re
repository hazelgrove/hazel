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
  | Comma => 0
  | LessThan => 1
  | GreaterThan => 1
  | Equals => 1
  | FLessThan => 1
  | FGreaterThan => 1
  | FEquals => 1
  | Cons => 2
  | Plus => 3
  | Minus => 3
  | FPlus => 3
  | FMinus => 3
  | And => 4
  | Or => 4
  | Times => 5
  | Divide => 5
  | FTimes => 5
  | FDivide => 5
  | Space => 6;

type associativity =
  | Left
  | Right;

let associativity =
  fun
  | Cons => Right
  | _ => Left;
