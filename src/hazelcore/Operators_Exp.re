open Sexplib.Std;

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
  | UserOp(string);

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
  | UserOp(op) => op;

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
  | _ => None;

let is_Space =
  fun
  | Space => true
  | _ => false;

let operator_of_char =
  fun
  | '+' => Plus
  | '-' => Minus
  | '*' => Times
  | '=' => Equals
  | '&' => And
  | '|' => Or
  | '<' => LessThan
  | '>' => GreaterThan
  | _ =>
    failwith("Error: Create user defined operator with an invalid symbol.");

let first_op_of_user_op = (op: string) => {
  switch (String.length(op)) {
  | 0 =>
    failwith(
      "Error: invalid operator symbol provided to user defined operator.",
    )
  | _ => operator_of_char(op.[0])
  };
};

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
  | UserOp(op) => {
      precedence(first_op_of_user_op(op));
    };

let precedence_const = 0;
let precedence_Ap = 1;
let precedence_max = 10;

let rec associativity =
  fun
  | Cons => Associativity.Right
  | And => Associativity.Right
  | Or => Associativity.Right
  | UserOp(op) => associativity(first_op_of_user_op(op))
  | _ => Associativity.Left;
