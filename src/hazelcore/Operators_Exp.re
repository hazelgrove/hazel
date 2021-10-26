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
  | Caret
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
  | Caret => "^"
  | Comma => ","
  | Cons => "::"
  | And => "&&"
  | Or => "||";

let is_Space =
  fun
  | Space => true
  | _ => false;

let precedence =
  fun
  | Comma => 2
  | Or => 3
  | And => 4
  | LessThan => 5
  | GreaterThan => 5
  | Equals => 5
  | FLessThan => 5
  | FGreaterThan => 5
  | FEquals => 5
  | Caret => 6
  | Cons => 7
  | Plus => 8
  | Minus => 8
  | FPlus => 8
  | FMinus => 8
  | Times => 9
  | Divide => 9
  | FTimes => 9
  | FDivide => 9
  | Space => 10;

let precedence_const = 0;
let precedence_Ap = 1;
let precedence_max = 10;

let associativity =
  fun
  | Cons => Associativity.Right
  | And => Associativity.Right
  | Or => Associativity.Right
  | _ => Associativity.Left;
