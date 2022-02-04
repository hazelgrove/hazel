open Format;

exception NotImplemented;

let rec emit_expr = (d: IHExp.t) => {
  switch (d) {
  | BoundVar(v) => v
  | Let(dp, d1, d2) =>
    Printf.sprintf(
      "let %s = %s\n%s",
      emit_pat(dp),
      emit_expr(d1),
      emit_expr(d2),
    )
  | Lam(dp, _, d1) =>
    Printf.sprintf("((%s) => {%s})", emit_pat(dp), emit_expr(d1))
  // TODO: Do this without copying Evaluator.subst_var?
  | FixF(_var, _, _d1) =>
    /* let d1s = */
    /* Evaluator.subst_var( */
    /* IHExp.BoundVar(String.concat(var, ["()"])), */
    /* var, */
    /* d1, */
    /* ); */
    /* Printf.sprintf("{let rec %s = () => {%s}}", var, emit_expr(d1s)); */
    raise(NotImplemented)
  | Ap(d1, d2) => Printf.sprintf("%s(%s)", emit_expr(d1), emit_expr(d2))
  | BoolLit(b) => b ? "true" : "false"
  | IntLit(i) => sprintf("%i", i)
  | FloatLit(f) => sprintf("%f", f)
  | BinBoolOp(op, d1, d2) =>
    sprintf("(%s %s %s)", emit_expr(d1), emit_bool_op(op), emit_expr(d2))
  | BinIntOp(op, d1, d2) =>
    // TODO: this is wrong
    sprintf("(%s %s %s)", emit_expr(d1), emit_int_op(op), emit_expr(d2))
  | BinFloatOp(op, d1, d2) =>
    sprintf("(%s %s %s)", emit_expr(d1), emit_float_op(op), emit_expr(d2))
  | Pair(d1, d2) => sprintf("(%s, %s)", emit_expr(d1), emit_expr(d2))
  | ConsistentCase(Case(d1, lr, _)) =>
    let rules = lr |> List.map(emit_rule) |> String.concat("");
    sprintf("match (%s){\n%s}", emit_expr(d1), rules);
  | ListNil(_) => sprintf("[]")
  | Cons(d1, d2) => sprintf("[{%s}, ...%s]", emit_expr(d1), emit_expr(d2))
  | Inj(_, side, d1) =>
    switch (side) {
    | L => sprintf("L(%s)", emit_expr(d1))
    | R => sprintf("R(%s)", emit_expr(d1))
    }
  | Triv => "void"
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | InconsistentBranches(_)
  | Cast(_)
  | FailedCast(_)
  | InvalidOperation(_) => raise(NotImplemented)
  };
}
and emit_rule = (r: IHExp.rule) => {
  let Rule(dp, d0) = r;
  sprintf("%s => %s\n", emit_pat(dp), emit_expr(d0));
}
and emit_pat = (dp: IHPat.t) => {
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_)
  | Inj(_)
  | ListNil
  | Cons(_)
  | Triv
  | Ap(_) => raise(NotImplemented)
  | Wild => "_"
  | Var(v) => v
  | IntLit(i) => sprintf("%i", i)
  | FloatLit(f) => sprintf("%f", f)
  | BoolLit(b) => b ? "true" : "false"
  | Pair(dp1, dp2) => sprintf("(%s, %s)", emit_pat(dp1), emit_pat(dp2))
  };
}
and emit_bool_op = (op: IHExp.BinBoolOp.t) => {
  switch (op) {
  | And => "&&"
  | Or => "||"
  };
}
and emit_int_op = (op: IHExp.BinIntOp.t) => {
  switch (op) {
  | Minus => "-"
  | Plus => "+"
  | Times => "*"
  | Divide => "/"
  | LessThan => "<"
  | GreaterThan => ">"
  | Equals => "=="
  };
}
and emit_float_op = (op: IHExp.BinFloatOp.t) => {
  switch (op) {
  | FPlus => "+"
  | FMinus => "-"
  | FTimes => "*"
  | FDivide => "/"
  | FLessThan => "<"
  | FGreaterThan => ">"
  | FEquals => "=="
  };
};

let emit = (d: IHExp.t) => {
  let s = "enum HazelSum<a, b> { L(a), R(b) }\n";
  sprintf("%s%s", s, emit_expr(d));
};
