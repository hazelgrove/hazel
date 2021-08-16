[@deriving (sexp, show)]
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
  | Cons => 6
  | Plus => 7
  | Minus => 7
  | FPlus => 7
  | FMinus => 7
  | Times => 8
  | Divide => 8
  | FTimes => 8
  | FDivide => 8
  | Space => 9;

let precedence_const = 0;
let precedence_Ap = 1;
let precedence_max = 10;

let associativity =
  fun
  | Cons => Associativity.Right
  | And => Associativity.Right
  | Or => Associativity.Right
  | _ => Associativity.Left;
