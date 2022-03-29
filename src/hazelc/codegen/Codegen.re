let sprintf = Printf.sprintf;

module Consts = {
  let truelit = "true";
  let falselit = "false";
  let voidlit = "void";

  let prim_and_op = "&&";
  let prim_or_op = "||";
  let prim_plus_op = "+";
  let prim_minus_op = "-";
  let prim_times_op = "*";
  let prim_divide_op = "/";
  let prim_less_than_op = "<";
  let prim_greater_than_op = "<";
  let prim_equals_op = "<";

  let pat_wild = "_";
  let pat_list_nil = "[]";
  let pat_triv = "void";
};

/*
   Raise a CodegenError.Exception.
 */
let codegen_raise = err => raise(CodegenError.Exception(err));

let codegen_infix = (o1, op, o2) => sprintf("%s %s %s", o1, op, o2);
let codegen_lines = ss => ss |> String.concat("\n");
let codegen_sep = (delim, ss) => ss |> String.concat(delim);
let codegen_comma_sep = codegen_sep(", ");

let rec codegen = ((tb, b): Gir.program) =>
  [codegen_top_block(tb), codegen_block_nowrap(b)] |> codegen_lines

and codegen_top_block = (tb: Gir.top_block) => {
  tb |> List.map(codegen_top_statement) |> codegen_lines;
}

and codegen_top_statement = (tstmt: Gir.top_statement) =>
  switch (tstmt) {
  | Import(name, path) => codegen_import(name, path)
  | Decl(decl) => codegen_decl(decl)
  }

and codegen_import = (name: Var.t, path: string) =>
  // TODO: Relative import?
  sprintf("import %s from \"%s\"", name, path)

and codegen_decl = (decl: Gir.decl) => {
  switch (decl) {
  | Enum(name, type_vars, variants) =>
    let type_vars =
      List.length(type_vars) == 0
        ? "" : type_vars |> codegen_comma_sep |> sprintf("<%s>");
    let variants =
      variants
      |> List.map(((vname, vbody)) =>
           if (List.length(vbody) == 0) {
             vname;
           } else {
             vbody |> codegen_comma_sep |> sprintf("%s(%s)", vname);
           }
         )
      |> codegen_comma_sep;

    sprintf("enum %s%s { %s }", name, type_vars, variants);
  };
}

and codegen_block_nowrap = (b: Gir.block) => {
  b |> List.map(codegen_statement) |> codegen_lines;
}
and codegen_block = (b: Gir.block) => {
  let s = codegen_block_nowrap(b);
  sprintf("{ %s }", s);
}

and codegen_statement = (stmt: Gir.statement) =>
  switch (stmt) {
  | Let(params, e) =>
    let params = codegen_params(params);
    let e = codegen_expr(e);
    sprintf("let %s = %s", params, e);
  | LetRec(params, e) =>
    let params = codegen_params(params);
    let e = codegen_expr(e);
    sprintf("let rec %s = %s", params, e);
  | Expr(e) => codegen_expr(e)
  }

and codegen_expr = (e: Gir.expr) =>
  switch (e) {
  | BoolLit(b) => b ? Consts.truelit : Consts.falselit
  | IntLit(n) => string_of_int(n)
  // TODO: NaN?
  | FloatLit(f) => string_of_float(f)
  | BinOp(op, e1, e2) => codegen_bin_op(op, e1, e2)
  | List(es) =>
    let es = es |> List.map(codegen_expr) |> String.concat(", ");
    sprintf("[%s]", es);
  | Triv => Consts.voidlit
  | Var(var) => codegen_var(var)
  | Lam(params, e') => codegen_lam(params, e')
  | Ap(lam, args) => codegen_ap(lam, args)
  | Match(scrut, rules) => codegen_match(scrut, rules)
  | Block(b) => codegen_block(b)
  }
and codegen_args = (args: Gir.args) =>
  args |> List.map(codegen_expr) |> codegen_comma_sep

and codegen_bin_op = (op: Gir.bin_op, e1: Gir.expr, e2: Gir.expr) => {
  let op =
    switch (op) {
    | And => Consts.prim_and_op
    | Or => Consts.prim_or_op
    | Plus => Consts.prim_plus_op
    | Minus => Consts.prim_minus_op
    | Times => Consts.prim_times_op
    | Divide => Consts.prim_divide_op
    | LessThan => Consts.prim_less_than_op
    | GreaterThan => Consts.prim_greater_than_op
    | Equals => Consts.prim_equals_op
    | FPlus => Consts.prim_plus_op
    | FMinus => Consts.prim_minus_op
    | FTimes => Consts.prim_times_op
    | FDivide => Consts.prim_divide_op
    | FLessThan => Consts.prim_less_than_op
    | FGreaterThan => Consts.prim_greater_than_op
    | FEquals => Consts.prim_equals_op
    };
  let e1 = codegen_expr(e1);
  let e2 = codegen_expr(e2);
  sprintf("%s %s %s", e1, op, e2);
}

and codegen_var = (var: Gir.var) =>
  switch (var) {
  | Named(var) => codegen_sep(".", var)
  | Tmp(n) => sprintf("t%d", n)
  }
and codegen_params = (ps: Gir.params) =>
  ps |> List.map(codegen_var) |> String.concat(", ")

and codegen_lam = (params: Gir.params, e': Gir.expr) => {
  let params = codegen_params(params);
  let e' = codegen_expr(e');
  sprintf("(%s) => { %s }", params, e');
}

and codegen_ap = (lam, args) => {
  let lam = codegen_expr(lam);
  let args = codegen_args(args);
  sprintf("%s(%s)", lam, args);
}

and codegen_match = (scrut: Gir.expr, rules: list(Gir.rule)) => {
  let scrut = codegen_expr(scrut);
  let rules = rules |> List.map(codegen_rule) |> codegen_comma_sep;
  sprintf("match (%s) { %s }", scrut, rules);
}
and codegen_rule = (rule: Gir.rule) => {
  switch (rule) {
  | Rule(p, rhs) =>
    let p = codegen_pat(p);
    let rhs = codegen_expr(rhs);
    sprintf("%s => %s", p, rhs);
  };
}

and codegen_pat = (p: Gir.pat) => {
  switch (p) {
  | Wild => Consts.pat_wild
  // TODO: Check if var conflicts with a keyword?
  | Var(var) => var
  | IntLit(i) => string_of_int(i)
  | FloatLit(f) => string_of_float(f)
  | BoolLit(b) => string_of_bool(b)
  | ListNil => Consts.pat_list_nil
  | Triv => Consts.pat_triv
  // TODO: Optimize this?
  | Cons(p1, p2) =>
    let p1 = codegen_pat(p1);
    let p2 = codegen_pat(p2);
    sprintf("[%s, ...%s]", p1, p2);
  | Pair(p1, p2) =>
    let p1 = codegen_pat(p1);
    let p2 = codegen_pat(p2);
    sprintf("(%s, %s)", p1, p2);
  };
};
