type expr =
        | BinaryOp of binary_op * expr * expr
        | Constant of constant
        (* FIXME: TEMPORARY *)
        | Error of constant * constant
and constant =
        | Int of int
        | Unit
and binary_op =
        | Plus
        | Minus
        | LT
        | GT
        | EQ

let string_of_binop e = match e with
        | Plus -> "plus"
        | Minus -> "minus"
        | LT -> "lt"
        | GT -> "gt"
        | EQ -> "eq"

let rec string_of_expr e = match e with
        | BinaryOp (op, e1, e2) ->
                "BinOp[ "^(string_of_binop(op))^": "^(string_of_expr(e1))^", "^(string_of_expr(e2))^"] "
        | Constant (Int i) -> "Int[ "^(string_of_int i)^" ]"
        | Error ((Int l), (Int c)) -> "Error at line: "^(string_of_int l)^", col: "^(string_of_int c)^". "
        | _ -> "something"
