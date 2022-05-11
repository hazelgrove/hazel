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

let print_infix = (o1, op, o2) => sprintf("%s %s %s", o1, op, o2);
let print_lines = ss => ss |> String.concat("\n");
let print_sep = (delim, ss) => ss |> String.concat(delim);
let print_comma_sep = print_sep(", ");

let rec print = ((tb, b): GrainIR.prog) =>
  [print_top_block(tb), print_block_nowrap(b)] |> print_lines

and print_top_block = (tb: GrainIR.top_block) => {
  tb |> List.map(print_top_statement) |> print_lines;
}

and print_top_statement = (tstmt: GrainIR.top_statement) =>
  switch (tstmt) {
  | Import(name, path) => print_import(name, path)
  | Decl(decl) => print_decl(decl)
  }

and print_import = (name: Var.t, path: string) =>
  // TODO: Relative import?
  sprintf("import %s from \"%s\"", name, path)

and print_decl = (decl: GrainIR.decl) =>
  switch (decl) {
  | Enum(en) => print_enum(en)
  }

and print_enum = ({name, type_vars, variants}: GrainIR.enum) => {
  let type_vars =
    List.length(type_vars) == 0
      ? "" : type_vars |> print_comma_sep |> sprintf("<%s>");
  let variants =
    variants
    |> List.map((variant: GrainIR.enum_variant) =>
         if (List.length(variant.params) == 0) {
           variant.ctor;
         } else {
           variant.params
           |> print_comma_sep
           |> sprintf("%s(%s)", variant.ctor);
         }
       )
    |> print_comma_sep;

  sprintf("enum %s%s { %s }", name, type_vars, variants);
}

and print_block_nowrap = (b: GrainIR.block) => {
  b |> List.map(print_statement) |> print_lines;
}
and print_block = (b: GrainIR.block) => {
  let s = print_block_nowrap(b);
  sprintf("{ %s }", s);
}

and print_statement = (stmt: GrainIR.statement) =>
  switch (stmt) {
  | Let(params, e) =>
    let params = print_params(params);
    let e = print_expr(e);
    sprintf("let %s = %s", params, e);
  | LetRec(params, e) =>
    let params = print_params(params);
    let e = print_expr(e);
    sprintf("let rec %s = %s", params, e);
  | Expr(e) => print_expr(e)
  }

and print_expr = (e: GrainIR.expr) =>
  switch (e) {
  | BoolLit(b) => b ? Consts.truelit : Consts.falselit
  | IntLit(n) => string_of_int(n)
  // TODO: NaN?
  | FloatLit(f) => string_of_float(f)
  | BinOp(op, e1, e2) => print_bin_op(op, e1, e2)
  | List(es) =>
    let es = es |> List.map(print_expr) |> String.concat(", ");
    sprintf("[%s]", es);
  | Triv => Consts.voidlit
  | Cons(e1, e2) => print_cons(e1, e2)
  | Tuple(els) => print_tuple(els)
  | Var(var) => print_var(var)
  | Lam(params, e') => print_lam(params, e')
  | Ap(lam, args) => print_ap(lam, args)
  | Ctor(ctor, args) => print_ctor(ctor, args)
  | Match(scrut, rules) => print_match(scrut, rules)
  | Block(b) => print_block(b)
  }

and print_args = (args: GrainIR.args) =>
  args |> List.map(print_expr) |> print_comma_sep

and print_bin_op = (op: GrainIR.bin_op, e1: GrainIR.expr, e2: GrainIR.expr) => {
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
  let e1 = print_expr(e1);
  let e2 = print_expr(e2);
  print_infix(e1, op, e2);
}

and print_cons = (e1: GrainIR.expr, e2: GrainIR.expr) => {
  let e1 = print_expr(e1);
  let e2 = print_expr(e2);
  sprintf("[%s, ...%s]", e1, e2);
}

and print_tuple = (els: list(GrainIR.expr)) => {
  let els = els |> List.map(print_expr) |> print_comma_sep;
  sprintf("(%s)", els);
}

and print_var = (var: Var.t) => var
and print_params = (ps: GrainIR.params) => ps |> String.concat(", ")

and print_lam = (params: GrainIR.params, e': GrainIR.expr) => {
  let params = print_params(params);
  let e' = print_expr(e');
  sprintf("(%s) => { %s }", params, e');
}

and print_ap = (lam: GrainIR.expr, args: GrainIR.args) => {
  let lam = print_expr(lam);
  let args = print_args(args);
  sprintf("%s(%s)", lam, args);
}

and print_ctor = (ctor: Var.t, args: GrainIR.args) => {
  let ctor = print_var(ctor);
  let args = print_args(args);
  sprintf("%s(%s)", ctor, args);
}

and print_match = (scrut: GrainIR.expr, rules: list(GrainIR.rule)) => {
  let scrut = print_expr(scrut);
  let rules = rules |> List.map(print_rule) |> print_comma_sep;
  sprintf("match (%s) { %s }", scrut, rules);
}
and print_rule = (rule: GrainIR.rule) => {
  switch (rule) {
  | Rule(p, rhs) =>
    let p = print_pat(p);
    let rhs = print_expr(rhs);
    sprintf("%s => %s", p, rhs);
  };
}

and print_pat = (p: GrainIR.pat) => {
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
    let p1 = print_pat(p1);
    let p2 = print_pat(p2);
    sprintf("[%s, ...%s]", p1, p2);
  | Pair(p1, p2) =>
    let p1 = print_pat(p1);
    let p2 = print_pat(p2);
    sprintf("(%s, %s)", p1, p2);
  };
};
