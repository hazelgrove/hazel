open Format;

exception NotImplemented;

module Program = {
  type t = {
    preamble: list(string),
    statements: list(string),
    tmp_counter: int,
  };

  let mk =
      (
        ~preamble: list(string),
        ~statements: list(string),
        ~tmp_counter: int,
      ) => {
    {preamble, statements, tmp_counter};
  };
  let sum_t = "enum HazelSum<a, b> { L(a), R(b) }";
  let empty = mk(~preamble=[], ~statements=[], ~tmp_counter=0);
  let top_empty = mk(~preamble=[sum_t], ~statements=[], ~tmp_counter=0);

  let assign_tmp_var = (prog: t, exp: string) => {
    let var = sprintf("tmp%d", prog.tmp_counter);
    let newstmt = sprintf("let %s = %s", var, exp);
    (
      var,
      {
        ...prog,
        tmp_counter: prog.tmp_counter + 1,
        statements: prog.statements @ [newstmt],
      },
    );
  };

  let add_statement = (prog: t, stmt: string) => {
    {...prog, statements: prog.statements @ [stmt]};
  };

  let to_string = (prog: t) => {
    String.concat("\n", prog.preamble @ prog.statements);
  };
};

let rec translate_exp = (d: IHExp.t, prog: Program.t) => {
  // return the variable name corresponding to this expression
  //   and Program.t
  switch (d) {
  | BoundVar(v) => (v, prog)
  | Let(dp, d1, d2) =>
    let (v1, prog) = translate_exp(d1, prog);
    let stmt1 = Printf.sprintf("let %s = %s", translate_pat(dp), v1);
    let prog = Program.add_statement(prog, stmt1);
    let (v2, prog) = translate_exp(d2, prog);
    (v2, prog);
  | Lam(dp, _, d1) =>
    let (rval, iprog) = translate_exp(d1, Program.empty);
    let iprog = Program.add_statement(iprog, rval);
    let exp =
      sprintf("(%s) => {%s}", translate_pat(dp), Program.to_string(iprog));
    Program.assign_tmp_var(prog, exp);
  // // TODO: Do this without copying Evaluator.subst_var?
  | LetRec(var, _, dp, d1, dlet) =>
    let (rval, iprog) = translate_exp(d1, Program.empty);
    let iprog = Program.add_statement(iprog, rval);
    let letstmt =
      Printf.sprintf(
        "let rec %s = (%s) => {%s}",
        var,
        translate_pat(dp),
        Program.to_string(iprog),
      );
    let prog = Program.add_statement(prog, letstmt);
    let (vlet, prog) = translate_exp(dlet, prog);
    (vlet, prog);
  | Ap(d1, d2) =>
    let (v1, prog) = translate_exp(d1, prog);
    let (v2, prog) = translate_exp(d2, prog);
    let exp = sprintf("%s(%s)", v1, v2);
    Program.assign_tmp_var(prog, exp);
  | ApBuiltin(_, _) => raise(NotImplemented)
  | BoolLit(b) => (b ? "true" : "false", prog)
  | IntLit(i) => (sprintf("%d", i), prog)
  | FloatLit(f) =>
    (sprintf("%f", f), prog);
    Printf.sprintf("%s(%s)", translate_exp(d1), translate_exp(d2));
  | BinBoolOp(op, d1, d2) =>
    let (v1, prog) = translate_exp(d1, prog);
    let (v2, prog) = translate_exp(d2, prog);
    let exp =
      switch (op) {
      | And => sprintf("Hazel.and(%s, %s)", v1, v2)
      | Or => sprintf("Hazel.or(%s, %s)", v1, v2)
      };
    Program.assign_tmp_var(prog, exp);
  // | BinIntOp(op, d1, d2) =>
  //   // TODO: this is wrong
  //   sprintf(
  //     "(%s %s %s)",
  //     translate_exp(d1),
  //     translate_int_op(op),
  //     translate_exp(d2),
  //   )
  // | BinFloatOp(op, d1, d2) =>
  //   sprintf(
  //     "(%s %s %s)",
  //     translate_exp(d1),
  //     translate_float_op(op),
  //     translate_exp(d2),
  //   )
  | Pair(d1, d2) =>
    let (v1, prog) = translate_exp(d1, prog);
    let (v2, prog) = translate_exp(d2, prog);
    let exp = sprintf("(%s, %s)", v1, v2);
    Program.assign_tmp_var(prog, exp);
  | ConsistentCase(Case(d1, lr, _)) =>
    let (v1, prog) = translate_exp(d1, prog);
    let rules = lr |> List.map(translate_rule) |> String.concat("");
    let exp = sprintf("match (%s){\n%s}", v1, rules);
    Program.assign_tmp_var(prog, exp);
  | ListNil(_) => ("[]", prog)
  | Cons(d1, d2) =>
    let (v1, prog) = translate_exp(d1, prog);
    let (v2, prog) = translate_exp(d2, prog);
    let exp = sprintf("[%s, ...%s]", v1, v2);
    Program.assign_tmp_var(prog, exp);
  | Inj(_, side, d1) =>
    let (v1, prog) = translate_exp(d1, prog);
    let exp =
      switch (side) {
      | L => sprintf("L(%s)", v1)
      | R => sprintf("R(%s)", v1)
      };
    Program.assign_tmp_var(prog, exp);
  | Triv => ("void", prog)
  | EmptyHole(metavar, _, _) =>
    let exp = sprintf("Hazel.buildEmptyHole(%d)", metavar);
    Program.assign_tmp_var(prog, exp);
  // | NonEmptyHole(_)
  // | Keyword(_)
  // | FreeVar(_)
  // | InvalidText(_)
  // | InconsistentBranches(_)
  // | Cast(_)
  // | FailedCast(_)
  // | InvalidOperation(_) => raise(NotImplemented)
  | _ => raise(NotImplemented)
  };
}
and translate_rule = (r: IHExp.rule) => {
  let Rule(dp, d0) = r;
  let (vret, iprog) = translate_exp(d0, Program.empty);
  sprintf(
    "%s => {%s\n%s}\n",
    translate_pat(dp),
    Program.to_string(iprog),
    vret,
  );
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
};
// and translate_bool_op = (op: IHExp.BinBoolOp.t) => {
//   switch (op) {
//   | And => "&&"
//   | Or => "||"
//   };
// }
// and translate_int_op = (op: IHExp.BinIntOp.t) => {
//   switch (op) {
//   | Minus => "-"
//   | Plus => "+"
//   | Times => "*"
//   | Divide => "/"
//   | LessThan => "<"
//   | GreaterThan => ">"
//   | Equals => "=="
//   };
// }
// and translate_float_op = (op: IHExp.BinFloatOp.t) => {
//   switch (op) {
//   | FPlus => "+"
//   | FMinus => "-"
//   | FTimes => "*"
//   | FDivide => "/"
//   | FLessThan => "<"
//   | FGreaterThan => ">"
//   | FEquals => "=="
//   };
// };

let translate = (d: IHExp.t) => {
  let (vret, prog) = translate_exp(d, Program.top_empty);
  sprintf("%s\n%s", Program.to_string(prog), vret);
};
