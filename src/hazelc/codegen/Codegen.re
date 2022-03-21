exception NotImplemented;

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

  let hazel_mod = "Hazel";
  let hazel_mk = (f: string) => hazel_mod ++ "." ++ f;

  let hazel_and = hazel_mk("and");
  let hazel_or = hazel_mk("or");

  let hazel_plus = hazel_mk("plus");
  let hazel_minus = hazel_mk("minus");
  let hazel_times = hazel_mk("times");
  let hazel_divide = hazel_mk("divide");
  let hazel_less_than = hazel_mk("less_than");
  let hazel_greater_than = hazel_mk("greater_than");
  let hazel_equals = hazel_mk("equals");

  let hazel_fplus = hazel_mk("fplus");
  let hazel_fminus = hazel_mk("fminus");
  let hazel_ftimes = hazel_mk("ftimes");
  let hazel_fdivide = hazel_mk("fdivide");
  let hazel_fless_than = hazel_mk("fless_than");
  let hazel_fgreater_than = hazel_mk("fgreater_than");
  let hazel_fequals = hazel_mk("fequals");
};

let codegen_infix = (o1, op, o2) => sprintf("%s %s %s", o1, op, o2);

let rec codegen = (b: GHExp.t) => codegen_block(b)
and codegen_block = (b: GHExp.block) => {
  let s = b |> List.map(codegen_statement) |> String.concat("\n");
  sprintf("{ %s }", s);
}

and codegen_statement = (stmt: GHExp.statement) =>
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

and codegen_expr = (e: GHExp.expr) =>
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
  | Lam(params, e') => raise(NotImplemented)
  | Ap(lam, args) => raise(NotImplemented)
  | Match(e', rules) => raise(NotImplemented)
  }

and codegen_bin_op = (op: GHExp.bin_op, e1: GHExp.expr, e2: GHExp.expr) =>
  switch (op) {
  | Prim(op') => codegen_bin_op_prim(op', e1, e2)
  | Hazel(op') => codegen_bin_op_hazel(op', e1, e2)
  }
and codegen_bin_op_prim = (op: GHExp.BinOp.op, e1: GHExp.expr, e2: GHExp.expr) => {
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
and codegen_bin_op_hazel =
    (op: GHExp.BinOp.op, e1: GHExp.expr, e2: GHExp.expr) => {
  let f =
    switch (op) {
    | And => Consts.hazel_and
    | Or => Consts.hazel_or
    | Plus => Consts.hazel_plus
    | Minus => Consts.hazel_minus
    | Times => Consts.hazel_times
    | Divide => Consts.hazel_divide
    | LessThan => Consts.hazel_less_than
    | GreaterThan => Consts.hazel_greater_than
    | Equals => Consts.hazel_equals
    | FPlus => Consts.hazel_fplus
    | FMinus => Consts.hazel_fminus
    | FTimes => Consts.hazel_ftimes
    | FDivide => Consts.hazel_fdivide
    | FLessThan => Consts.hazel_fless_than
    | FGreaterThan => Consts.hazel_fgreater_than
    | FEquals => Consts.hazel_fequals
    };
  codegen_expr(Ap(Var(Named(f)), [e1, e2]));
}

and codegen_var = (var: GHExp.var) =>
  switch (var) {
  | Named(var) => var
  | Tmp(n) => sprintf("t%d", n)
  }
and codegen_params = (ps: GHExp.params) =>
  ps |> List.map(codegen_var) |> String.concat(", ");
