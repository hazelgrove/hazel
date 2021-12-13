open Format;

exception NotImplemented;

let print_bool_op = (p: DHExp.BinBoolOp.t) => {
  switch (p) {
  | And => "&&"
  | Or => "||"
  };
};

let print_int_op = (p: DHExp.BinIntOp.t) => {
  switch (p) {
  | Minus => "-"
  | Plus => "+"
  | Times => "*"
  | Divide => "/"
  | LessThan => "<"
  | GreaterThan => ">"
  | Equals => "=="
  };
};

let print_float_op = (p: DHExp.BinFloatOp.t) => {
  switch (p) {
  | FPlus => "+"
  | FMinus => "-"
  | FTimes => "*"
  | FDivide => "/"
  | FLessThan => "<"
  | FGreaterThan => ">"
  | FEquals => "=="
  };
};

let rec print_pattern = (p: DHPat.t) => {
  switch (p) {
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
  | Pair(p1, p2) =>
    sprintf("(%s, %s)", print_pattern(p1), print_pattern(p2))
  };
};

let rec print_expression = (d: DHExp.t) => {
  switch (d) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_) => raise(NotImplemented)
  | BoundVar(v) => v
  | Let(dp, d1, d2) =>
    Printf.sprintf(
      "let %s = %s\n%s",
      print_pattern(dp),
      print_expression(d1),
      print_expression(d2),
    )
  | Lam(dp, _, d1) =>
    Printf.sprintf(
      "((%s) => {%s})",
      print_pattern(dp),
      print_expression(d1),
    )
  | FixF(var, _, d1) =>
    let d1s =
      Evaluator.subst_var(
        DHExp.BoundVar(String.concat(var, ["()"])),
        var,
        d1,
      );
    Printf.sprintf("{let rec %s = () => {%s}}", var, print_expression(d1s));
  | Ap(d1, d2) =>
    Printf.sprintf("%s(%s)", print_expression(d1), print_expression(d2))
  | BoolLit(b) => b ? "true" : "false"
  | IntLit(i) => sprintf("%i", i)
  | FloatLit(f) => sprintf("%f", f)
  | BinBoolOp(op, d1, d2) =>
    sprintf(
      "(%s %s %s)",
      print_expression(d1),
      print_bool_op(op),
      print_expression(d2),
    )
  | BinIntOp(op, d1, d2) =>
    // TODO: this is wrong
    sprintf(
      "(%s %s %s)",
      print_expression(d1),
      print_int_op(op),
      print_expression(d2),
    )
  | BinFloatOp(op, d1, d2) =>
    sprintf(
      "(%s %s %s)",
      print_expression(d1),
      print_float_op(op),
      print_expression(d2),
    )
  | Pair(d1, d2) =>
    sprintf("(%s, %s)", print_expression(d1), print_expression(d2))
  | ConsistentCase(Case(d1, lr, _)) =>
    let rules = lr |> List.map(print_rule) |> String.concat("");
    sprintf("match (%s){\n%s}", print_expression(d1), rules);
  | ListNil(_) => sprintf("[]")
  | Cons(d1, d2) =>
    sprintf("[{%s}, ...%s]", print_expression(d1), print_expression(d2))
  | Inj(_, side, d1) =>
    switch (side) {
    | L => sprintf("L(%s)", print_expression(d1))
    | R => sprintf("R(%s)", print_expression(d1))
    }
  | _ => raise(NotImplemented)
  //   | Triv
  //   | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  //   | Cast(t, HTyp.t, HTyp.t)
  //   | FailedCast(t, HTyp.t, HTyp.t)
  //   | InvalidOperation(t, InvalidOperationError.t)
  };
}
and print_rule = (r: DHExp.rule) => {
  let Rule(dp, d0) = r;
  sprintf("%s => %s\n", print_pattern(dp), print_expression(d0));
};

let print_grain = (d: DHExp.t) => {
  let s = "enum HazelSum<a, b> { L(a), R(b) }\n";
  sprintf("%s%s", s, print_expression(d));
};
