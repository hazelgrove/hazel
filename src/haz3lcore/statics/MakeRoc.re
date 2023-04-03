let rec generate_code = (t: TermRoc.UExp.t, i: int): string =>
  switch (t) {
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
  | ListLit(lst) => "[" ++ list_to_string(lst, i) ++ "]"
  // | Tag(tag) =>
  | Fun(pat, body) =>
    "\\ "
    ++ get_code_pat(pat, i)
    ++ " -> "
    ++ "\n"
    ++ String.make(i + 1, ' ')
    ++ generate_code(body, i + 1)
  | Record(t) => get_record_term(t, i)
  // | Tuple(l) => l |> List.map(generate_code) |> String.concat(", ")
  | Var(token) => token
  | Assign(p, def) => get_code_pat(p, i) ++ " = " ++ generate_code(def, i)
  | Ap(fn, arg) =>
    generate_code(fn, i) ++ " (" ++ generate_code(arg, i) ++ ")"
  | If(cond, true_branch, false_branch) =>
    "if "
    ++ generate_code(cond, i)
    ++ " then "
    ++ generate_code(true_branch, i)
    ++ " else "
    ++ generate_code(false_branch, i)
  | SeqIndent(expr1, expr2) =>
    generate_code(expr1, i + 1)
    ++ "\n"
    ++ String.make(i, ' ')
    ++ generate_code(expr2, i)
  | SeqMatchIndent(expr1, expr2) =>
    generate_code(expr1, i + 1)
    ++ "\n"
    ++ String.make(i + 1, ' ')
    ++ generate_code(expr2, i + 1)
  | Seq(expr1, expr2) =>
    generate_code(expr1, i)
    ++ "\n"
    ++ String.make(i, ' ')
    ++ generate_code(expr2, i)
  | SeqList(l) => seqlist_to_string(l, i)
  | SeqNoBreak(l) => seqnobreak_to_string(l, i)
  | Expect(expr) => "expect " ++ generate_code(expr, i)
  | Parens(expr) => "(" ++ generate_code(expr, i) ++ ")"
  | UnOp(op, operand) => get_code_un(op) ++ generate_code(operand, i)
  | BinOp(op, lhs, rhs) =>
    generate_code(lhs, i)
    ++ " "
    ++ get_code_bin(op)
    ++ " "
    ++ generate_code(rhs, i)
  | Match(t, l) =>
    "when " ++ generate_code(t, i) ++ " is\n" ++ get_match_body(l, i + 1)
  | TypeAnn(v, typ) => v ++ " : " ++ get_code_typ(typ)
  }

and get_code_pat = (t: TermRoc.UPat.t, i: int): string =>
  switch (t) {
  | Wild => "_"
  | Rest => ".."
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Int(n) => string_of_int(n)
  | Float(f) => string_of_float(f)
  | String(s) => s
  | ListLit(lst) => "[" ++ list_to_string_pat(lst, i) ++ "]"
  // | Tag(_) =>
  | Var(token) => token
  | Record(pat) => get_record_pat(pat, i)
  // | Tuple(l) => l |> List.map(get_code_pat) |> String.concat(", ")
  | Parens(expr) => "(" ++ get_code_pat(expr, i) ++ ")"
  }

and get_code_typ = (t: TermRoc.UTyp.t): string =>
  switch (t) {
  | Int => "I64"
  | Float => "F64"
  | Bool => "Bool"
  | String => "Str"
  | List(t) => "List " ++ get_code_typ(t)
  | Var(s) => s
  | Arrow(t1, t2) => get_code_typ(t1) ++ " -> " ++ get_code_typ(t2)
  | Record(l) => get_record_typ(l)
  // | Tuple(l) => l |> List.map(get_code_typ) |> String.concat(", ")
  | Parens(expr) => "(" ++ get_code_typ(expr) ++ ")"
  }
and list_to_string = (lst: list(TermRoc.UExp.t), i: int) => {
  switch (lst) {
  | [] => ""
  | [x] => generate_code(x, i)
  | [x, ...xs] => generate_code(x, i) ++ ", " ++ list_to_string(xs, i)
  };
}
and list_to_string_pat = (lst: list(TermRoc.UPat.t), i: int) => {
  switch (lst) {
  | [] => ""
  | [x] => get_code_pat(x, i)
  | [x, ...xs] => get_code_pat(x, i) ++ ", " ++ list_to_string_pat(xs, i)
  };
}
and seqlist_to_string = (lst: list(TermRoc.UExp.t), i: int) => {
  switch (lst) {
  | [] => ""
  | [x] => generate_code(x, i)
  | [x, ...xs] =>
    generate_code(x, i)
    ++ "\n"
    ++ String.make(i, ' ')
    ++ seqlist_to_string(xs, i)
  };
}

and seqnobreak_to_string = (lst: list(TermRoc.UExp.t), i: int) => {
  switch (lst) {
  | [] => ""
  | [x] => generate_code(x, i)
  | [x, ...xs] => generate_code(x, i) ++ " " ++ seqnobreak_to_string(xs, i)
  };
}
and get_record_term = (terms: list(TermRoc.UExp.t), indent: int): string => {
  let termi =
    List.mapi(
      (i, t) => {
        "t" ++ string_of_int(i) ++ ": " ++ generate_code(t, indent)
      },
      terms,
    );
  let r = String.concat(", ", termi);
  "{" ++ r ++ "}";
}
and get_record_pat = (pats: list(TermRoc.UPat.t), indent: int): string => {
  let pati =
    List.mapi(
      (i, p) => {
        "t" ++ string_of_int(i) ++ ": " ++ get_code_pat(p, indent)
      },
      pats,
    );
  let r = String.concat(", ", pati);
  "{" ++ r ++ "}";
}
and get_record_typ = (typs: list(TermRoc.UTyp.t)): string => {
  let typi =
    List.mapi(
      (i, typ) => {"t" ++ string_of_int(i) ++ ": " ++ get_code_typ(typ)},
      typs,
    );
  let r = String.concat(", ", typi);
  "{" ++ r ++ "}";
}

and get_match_body =
    (branches: list((TermRoc.UPat.t, TermRoc.UExp.t)), i: int): string => {
  let branchi =
    List.map(
      ((p, t)) =>
        switch (p, t) {
        | (pat, output) =>
          String.make(i, ' ')
          ++ get_code_pat(pat, i)
          ++ " -> "
          ++ generate_code(output, i)
        },
      branches,
    );
  String.concat("\n", branchi);
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
