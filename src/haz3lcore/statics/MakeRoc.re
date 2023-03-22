let rec list_to_string = (lst: list(TermRoc.UExp.t)) => {
  switch (lst) {
  | [] => ""
  | [x] => generate_code(x)
  | [x, ...xs] => generate_code(x) ++ ", " ++ list_to_string(xs)
  };
}
and list_to_string_pat = (lst: list(TermRoc.UPat.t)) => {
  switch (lst) {
  | [] => ""
  | [x] => generate_code_pat(x)
  | [x, ...xs] => generate_code_pat(x) ++ ", " ++ list_to_string_pat(xs)
  };
}
and generate_code = (t: TermRoc.UExp.t): string =>
  switch (t.term) {
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Int(n) => string_of_int(n)
  | Float(f) => string_of_float(f)
  | String(s) => "\"" ++ s ++ "\""
  | ListLit(lst) => "[" ++ list_to_string(lst) ++ "]"
  | Tag(tag) => "#" ++ tag
  | Fun(pat, body) =>
    "fun " ++ generate_code_pat(pat) ++ " -> " ++ generate_code(body)
  | Record(_) => ""
  | Var(_) => ""
  | Assign(var, value) =>
    generate_code(var) ++ " in " ++ generate_code(value)
  | Ap(fn, arg) => "(" ++ generate_code(fn) ++ generate_code(arg) ++ ")"
  | If(cond, true_branch, false_branch) =>
    "if "
    ++ generate_code(cond)
    ++ " then "
    ++ generate_code(true_branch)
    ++ " else "
    ++ generate_code(false_branch)
  | Seq(expr1, expr2) =>
    generate_code(expr1) ++ "; " ++ generate_code(expr2)
  | Test(_) => ""
  | Parens(expr) => "(" ++ generate_code(expr) ++ ")"
  | UnOp(_, operand) => "-" ++ generate_code(operand)
  | BinOp(op, lhs, rhs) =>
    generate_code(lhs)
    ++ " "
    ++ generate_code_bin(op)
    ++ " "
    ++ generate_code(rhs)
  | Match(_, _) => ""
  }

and generate_code_pat = (t: TermRoc.UPat.t): string =>
  switch (t.term) {
  | Wild => "_"
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Int(n) => string_of_int(n)
  | Float(f) => string_of_float(f)
  | String(s) => "\"" ++ s ++ "\""
  | ListLit(lst) => "[" ++ list_to_string_pat(lst) ++ "]"
  | Tag(_) => ""
  | Var(_) => ""
  | Record(_) => ""
  | Parens(expr) => "(" ++ generate_code_pat(expr) ++ ")"
  | Ap(fn, arg) =>
    "(" ++ generate_code_pat(fn) ++ generate_code_pat(arg) ++ ")"
  | TypeAnn(_, _) => ""
  }

and generate_code_bin = (op: TermRoc.UExp.op_bin): string =>
  switch (op) {
  | Int_Float(op_bin_int_float) =>
    switch (op_bin_int_float) {
    | Plus => "+"
    | Minus => "-"
    | Times => "*"
    | Power => ""
    | Divide => "/"
    | LessThan => "<"
    | LessThanOrEqual => "<="
    | GreaterThan => ">"
    | GreaterThanOrEqual => ">="
    | Equals => "=="
    }
  | Bool(op_bin_bool) =>
    switch (op_bin_bool) {
    | And => "&&"
    | Or => "||"
    }
  | String(op_bin_string) =>
    switch (op_bin_string) {
    | Equals => "=="
    }
  };
