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
  | Space => Associativity.Left
  | Plus => Associativity.Left
  | Minus => Associativity.Left
  | Times => Associativity.Left
  | Divide => Associativity.Left
  | FPlus => Associativity.Left
  | FMinus => Associativity.Left
  | FTimes => Associativity.Left
  | FDivide => Associativity.Left
  | LessThan => Associativity.Left
  | GreaterThan => Associativity.Left
  | Equals => Associativity.Left
  | FLessThan => Associativity.Left
  | FGreaterThan => Associativity.Left
  | FEquals => Associativity.Left
  | Comma => Associativity.Left
  | Cons => Associativity.Right
  | And => Associativity.Right
  | Or => Associativity.Right
  | Caret => Associativity.Left;
