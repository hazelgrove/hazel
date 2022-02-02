open Format;

exception NotImplemented(string);

let rec trans_pattern = (p: DHPat.t) => {
  switch (p) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_)
  | Inj(_)
  | ListNil
  | Cons(_)
  | Triv
  | Ap(_) => raise(NotImplemented("pattern"))
  | Wild => "_"
  | Var(v) => v
  | IntLit(i) => sprintf("%i", i)
  | FloatLit(f) => sprintf("%f", f)
  | BoolLit(b) => b ? "true" : "false"
  | Pair(p1, p2) =>
    sprintf("(%s, %s)", trans_pattern(p1), trans_pattern(p2))
  };
};

let rec trans_expression = (d: CHExp.t) => {
  switch (d) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_) => raise(NotImplemented("indet"))
  | BoundVar(v) => v
  | Let(dp, d1, d2) =>
    Printf.sprintf(
      "{let %s = %s\n%s}",
      trans_pattern(dp),
      trans_expression(d1),
      trans_expression(d2),
    )
  | Lam(dp, _, d1) =>
    Printf.sprintf(
      "((%s) => {%s})",
      trans_pattern(dp),
      trans_expression(d1),
    )
  | RecFun(var, _, d1) =>
    Printf.sprintf("{let rec %s = () => {%s}}", var, trans_expression(d1))
  | Ap(d1, d2) =>
    Printf.sprintf("%s(%s)", trans_expression(d1), trans_expression(d2))
  | BoolLit(b) => b ? "Hazel.htrue" : "Hazel.hfalse"
  //| IntLit(i) => sprintf("%i", i)
  //| FloatLit(f) => sprintf("%f", f)
  | BinBoolOp(op, d1, d2) =>
    switch (op) {
    | And =>
      sprintf(
        "Hazel.and(%s, %s)",
        trans_expression(d1),
        trans_expression(d2),
      )
    | Or =>
      sprintf(
        "Hazel.or(%s, %s)",
        trans_expression(d1),
        trans_expression(d2),
      )
    }
  //   | BinIntOp(op, d1, d2) =>
  //     // TODO: this is wrong
  //     sprintf(
  //       "(%s %s %s)",
  //       print_expression(d1),
  //       print_int_op(op),
  //       print_expression(d2),
  //     )
  //   | BinFloatOp(op, d1, d2) =>
  //     sprintf(
  //       "(%s %s %s)",
  //       print_expression(d1),
  //       print_float_op(op),
  //       print_expression(d2),
  //     )
  | Pair(d1, d2) =>
    sprintf("(%s, %s)", trans_expression(d1), trans_expression(d2))
  //   | ConsistentCase(Case(d1, lr, _)) =>
  //     let rules = lr |> List.map(print_rule) |> String.concat("");
  //     sprintf("match (%s){\n%s}", trans_expression(d1), rules);
  | ListNil(_) => sprintf("[]")
  | Cons(d1, d2) =>
    sprintf("[{%s}, ...%s]", trans_expression(d1), trans_expression(d2))
  | Inj(_, side, d1) =>
    switch (side) {
    | L => sprintf("L(%s)", trans_expression(d1))
    | R => sprintf("R(%s)", trans_expression(d1))
    }
  | _ => raise(NotImplemented("cases & cast"))
  //   | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  //   | Cast(t, HTyp.t, HTyp.t)
  //   | FailedCast(t, HTyp.t, HTyp.t)
  //   | InvalidOperation(t, InvalidOperationError.t)
  };
};

let print_grain = (d: CHExp.t) => {
  //let s = "enum HazelSum<a, b> { L(a), R(b) }\n";
  let s = "import Hazel from \"hazel\"";
  let t = "Hazel.print_prog(result)";
  sprintf("%s\nlet result = %s\n%s", s, trans_expression(d), t);
};
