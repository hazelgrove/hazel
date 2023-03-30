let rec generate_code = (t: TermRoc.UExp.t): string =>
  switch (t.term) {
  | Bool(true) => "Bool.true"
  | Bool(false) => "Bool.false"
  | Int(n) => string_of_int(n)
  | Float(f) =>
    let f_str = string_of_float(f);
    let last_char = f_str.[String.length(f_str) - 1];
    if (last_char == '.') {
      f_str ++ "0";
    } else {
      f_str;
    };
  | String(s) => s
  | ListLit(lst) => "[" ++ list_to_string(lst) ++ "]"
  // what is the meaning of Tag
  | Tag(tag) => "#" ++ tag
  | Fun(pat, body) =>
    "\\ " ++ get_code_pat(pat) ++ " -> " ++ generate_code(body)
  | Record(t) => get_record_term(t)
  // | Tuple(l) => l |> List.map(generate_code) |> String.concat(", ")
  | Var(token) => token
  | Assign(p, def) => get_code_pat(p) ++ " = " ++ generate_code(def)
  | Ap(fn, arg) => generate_code(fn) ++ " (" ++ generate_code(arg) ++ ")"
  | If(cond, true_branch, false_branch) =>
    "if "
    ++ generate_code(cond)
    ++ " then "
    ++ generate_code(true_branch)
    ++ " else "
    ++ generate_code(false_branch)
  | Seq(expr1, expr2) =>
    generate_code(expr1)
    ++ "\n"
    ++ String.make(t.indent, ' ')
    ++ generate_code(expr2)
  | Expect(expr) => "expect " ++ generate_code(expr)
  | Parens(expr) => "(" ++ generate_code(expr) ++ ")"
  | UnOp(op, operand) => get_code_un(op) ++ generate_code(operand)
  | BinOp(op, lhs, rhs) =>
    generate_code(lhs)
    ++ " "
    ++ get_code_bin(op)
    ++ " "
    ++ generate_code(rhs)
  | Match(t, l) =>
    "when " ++ generate_code(t) ++ " is\n" ++ get_match_body(l, t.indent)
  | TypeAnn(v, typ) => v ++ ":" ++ get_code_typ(typ)
  }

and get_code_pat = (t: TermRoc.UPat.t): string =>
  switch (t.term) {
  | Wild => "_"
  | Rest => ".."
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Int(n) => string_of_int(n)
  | Float(f) => string_of_float(f)
  | String(s) => s
  | ListLit(lst) => "[" ++ list_to_string_pat(lst) ++ "]"
  | Tag(_) => ""
  | Var(token) => token
  | Record(pat) => get_record_pat(pat)
  // | Tuple(l) => l |> List.map(get_code_pat) |> String.concat(", ")
  | Parens(expr) => "(" ++ get_code_pat(expr) ++ ")"
  }

and get_code_typ = (t: TermRoc.UTyp.t): string =>
  switch (t) {
  | Int => "Int *"
  | IntV(s) => "Int " ++ s
  | Float => "Frac *"
  | FloatV(s) => "Frac " ++ s
  | Bool => "Bool"
  | String => "Str"
  | List(t) => "List" ++ get_code_typ(t)
  | Var(s) => s
  | Arrow(t1, t2) => get_code_typ(t1) ++ " -> " ++ get_code_typ(t2)
  | Record(l) => get_record_typ(l)
  // | Tuple(l) => l |> List.map(get_code_typ) |> String.concat(", ")
  | Parens(expr) => "(" ++ get_code_typ(expr) ++ ")"
  }
and list_to_string = (lst: list(TermRoc.UExp.t)) => {
  switch (lst) {
  | [] => ""
  | [x] => generate_code(x)
  | [x, ...xs] => generate_code(x) ++ ", " ++ list_to_string(xs)
  };
}
and list_to_string_pat = (lst: list(TermRoc.UPat.t)) => {
  switch (lst) {
  | [] => ""
  | [x] => get_code_pat(x)
  | [x, ...xs] => get_code_pat(x) ++ ", " ++ list_to_string_pat(xs)
  };
}
and get_record_term = (terms: list(TermRoc.UExp.t)): string => {
  let termi =
    List.mapi(
      (i, t) => {"t" ++ string_of_int(i) ++ ": " ++ generate_code(t)},
      terms,
    );
  let r = String.concat(", ", termi);
  "{ " ++ r ++ " }";
}
and get_record_pat = (pats: list(TermRoc.UPat.t)): string => {
  let pati =
    List.mapi(
      (i, p) => {"t" ++ string_of_int(i) ++ ": " ++ get_code_pat(p)},
      pats,
    );
  let r = String.concat(", ", pati);
  "{ " ++ r ++ " }";
}
and get_record_typ = (typs: list(TermRoc.UTyp.t)): string => {
  let typi =
    List.mapi(
      (i, typ) => {"t" ++ string_of_int(i) ++ ": " ++ get_code_typ(typ)},
      typs,
    );
  let r = String.concat(", ", typi);
  "{ " ++ r ++ " }";
}

and get_match_body =
    (branches: list((TermRoc.UPat.t, TermRoc.UExp.t)), i: int): string => {
  let branchi =
    List.map(
      ((p, t)) =>
        switch (p, t) {
        | (pat, output) =>
          "  " ++ get_code_pat(pat) ++ " -> " ++ generate_code(output)
        },
      branches,
    );
  String.concat(String.make(i, ' ') ++ "\n", branchi);
}

and get_code_un = (op: TermRoc.UExp.op_un): string =>
  switch (op) {
  | Int(Minus) => "-"
  }

and get_code_bin = (op: TermRoc.UExp.op_bin): string =>
  switch (op) {
  | Int_Float(op_bin_int_float) =>
    switch (op_bin_int_float) {
    | Plus => "+"
    | Minus => "-"
    | Times => "*"
    | Power => "^"
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
