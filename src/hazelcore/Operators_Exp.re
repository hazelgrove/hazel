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
  | Or => 1
  | And => 2
  | LessThan => 3
  | GreaterThan => 3
  | Equals => 3
  | FLessThan => 3
  | FGreaterThan => 3
  | FEquals => 3
  | Cons => 4
  | Plus => 5
  | Minus => 5
  | FPlus => 5
  | FMinus => 5
  | Times => 6
  | Divide => 6
  | FTimes => 6
  | FDivide => 6
  | Space => 7;

type associativity =
  | Left
  | Right;

let associativity =
  fun
  | Cons => Right
  | And => Right
  | Or => Right
  | _ => Left;
