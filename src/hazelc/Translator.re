open Format;

exception NotImplemented;

module Preamble = {
  let sum_t = "enum HazelSum<a, b> { L(a), R(b) }";

  let preamble = [sum_t];
  let to_string = () => String.concat("\n", preamble);
};

let rec translate_exp = (d: IHExp.t) => {
  switch (d) {
  | BoundVar(v) => v
  | Let(dp, d1, d2) =>
    Printf.sprintf(
      "let %s = %s\n%s",
      translate_pat(dp),
      translate_exp(d1),
      translate_exp(d2),
    )
  | Lam(dp, _, d1) =>
    Printf.sprintf("((%s) => {%s})", translate_pat(dp), translate_exp(d1))
  // TODO: Do this without copying Evaluator.subst_var?
  | FixF(_var, _, _d1) =>
    /* let d1s = */
    /* Evaluator.subst_var( */
    /* IHExp.BoundVar(String.concat(var, ["()"])), */
    /* var, */
    /* d1, */
    /* ); */
    /* Printf.sprintf("{let rec %s = () => {%s}}", var, translate_exp(d1s)); */
    raise(NotImplemented)
  | Ap(d1, d2) =>
    Printf.sprintf("%s(%s)", translate_exp(d1), translate_exp(d2))
  | BoolLit(b) => b ? "true" : "false"
  | IntLit(i) => sprintf("%i", i)
  | FloatLit(f) => sprintf("%f", f)
  | BinBoolOp(op, d1, d2) =>
    sprintf(
      "(%s %s %s)",
      translate_exp(d1),
      translate_bool_op(op),
      translate_exp(d2),
    )
  | BinIntOp(op, d1, d2) =>
    // TODO: this is wrong
    sprintf(
      "(%s %s %s)",
      translate_exp(d1),
      translate_int_op(op),
      translate_exp(d2),
    )
  | BinFloatOp(op, d1, d2) =>
    sprintf(
      "(%s %s %s)",
      translate_exp(d1),
      translate_float_op(op),
      translate_exp(d2),
    )
  | Pair(d1, d2) =>
    sprintf("(%s, %s)", translate_exp(d1), translate_exp(d2))
  | ConsistentCase(Case(d1, lr, _)) =>
    let rules = lr |> List.map(translate_rule) |> String.concat("");
    sprintf("match (%s){\n%s}", translate_exp(d1), rules);
  | ListNil(_) => sprintf("[]")
  | Cons(d1, d2) =>
    sprintf("[{%s}, ...%s]", translate_exp(d1), translate_exp(d2))
  | Inj(_, side, d1) =>
    switch (side) {
    | L => sprintf("L(%s)", translate_exp(d1))
    | R => sprintf("R(%s)", translate_exp(d1))
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
and translate_rule = (r: IHExp.rule) => {
  let Rule(dp, d0) = r;
  sprintf("%s => %s\n", translate_pat(dp), translate_exp(d0));
}
and translate_pat = (dp: IHPat.t) => {
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
  | Pair(dp1, dp2) =>
    sprintf("(%s, %s)", translate_pat(dp1), translate_pat(dp2))
  };
}
and translate_bool_op = (op: IHExp.BinBoolOp.t) => {
  switch (op) {
  | And => "&&"
  | Or => "||"
  };
}
and translate_int_op = (op: IHExp.BinIntOp.t) => {
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
and translate_float_op = (op: IHExp.BinFloatOp.t) => {
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

let translate_preamble = () => {
  Preamble.to_string();
};

let translate = (d: IHExp.t) => {
  sprintf("%s\n%s", translate_preamble(), translate_exp(d));
};
