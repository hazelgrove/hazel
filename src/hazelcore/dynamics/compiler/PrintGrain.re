open Format;

exception NotImplemented;

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
    Printf.sprintf("{let rec %s = () => {%s}", var, print_expression(d1s));
  | Ap(d1, d2) =>
    Printf.sprintf("%s %s", print_expression(d1), print_expression(d2))
  | BoolLit(b) => b ? "true" : "false"
  | IntLit(i) => sprintf("%i", i)
  | FloatLit(f) => sprintf("%f", f)
  | Pair(d1, d2) =>
    sprintf("(%s, %s)", print_expression(d1), print_expression(d2))
  | ConsistentCase(Case(d1, lr, _)) =>
    let rules = lr |> List.map(print_rule) |> String.concat("");
    sprintf("match (%s){\n%s}", print_expression(d1), rules);
  | _ => raise(NotImplemented)
  //   | BinBoolOp(BinBoolOp.t, t, t)
  //   | BinIntOp(BinIntOp.t, t, t)
  //   | BinFloatOp(BinFloatOp.t, t, t)
  //   | ListNil(HTyp.t)
  //   | Cons(t, t)
  //   | Inj(HTyp.t, InjSide.t, t)
  //   | Pair(t, t)
  //   | Triv
  //   | ConsistentCase(case)
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
