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
  | UserOp(Var.t);

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
  | UserOp(var) => var;

let string_to_operator =
  fun
  | "+" => Some(Plus)
  | "-" => Some(Minus)
  | "*" => Some(Times)
  | "/" => Some(Divide)
  | "<" => Some(LessThan)
  | ">" => Some(GreaterThan)
  | "==" => Some(Equals)
  | "::" => Some(Cons)
  | "&&" => Some(And)
  | "||" => Some(Or)
  | "," => Some(Comma)
  | var when Var.is_exp_operator(var) => Some(UserOp(var))
  | _ => None;

let is_Space =
  fun
  | Space => true
  | _ => false;

let is_UserOp =
  fun
  | UserOp(_) => true
  | _ => false;

let operator_of_char =
  fun
  | '+' => Plus
  | '-' => Minus
  | '*' => Times
  | '/' => Divide
  | '=' => Equals
  | '&' => And
  | '|' => Or
  | '<' => LessThan
  | '>' => GreaterThan
  | ':' => Cons
  | _ => failwith("Error creating user operator with an invalid symbol.");

// Todo: Refactor precedence into an ordered list, and insert user defined operators properly.
let rec precedence =
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
  | Space => 9
  | UserOp(op) when String.length(op) == 0 => {
      failwith("Error checking precedence of an empty user operator.");
    }
  | UserOp(op) => {
      precedence(operator_of_char(op.[0]));
    };

let precedence_const = 0;
let precedence_Ap = 1;
let precedence_max = 10;

let rec associativity =
  fun
  | Cons => Associativity.Right
  | And => Associativity.Right
  | Or => Associativity.Right
  | UserOp(op) when String.length(op) == 0 => {
      failwith("Error checking associativity of an empty user operator");
    }
  | UserOp(op) => associativity(operator_of_char(op.[0]))
  | _ => Associativity.Left;
