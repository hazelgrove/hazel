open Format;

exception NotImplemented;

let rec print_expression = (d: DHExp.t) => {
  switch (d) {
  | NonEmptyHole(_)
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_) => raise(NotImplemented)
  | EmptyHole(metavar, _, _) => sprintf("Hazel.buildEmptyHole(%d)", metavar)
  | BoolLit(b) => b ? "Hazel.htrue" : "Hazel.hfalse"
  | BinBoolOp(op, d1, d2) =>
    switch (op) {
    | And =>
      sprintf(
        "Hazel.and(%s, %s)",
        print_expression(d1),
        print_expression(d2),
      )
    | _ => raise(NotImplemented)
    }
  | _ => raise(NotImplemented)
  //   | Triv
  //   | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  //   | Cast(t, HTyp.t, HTyp.t)
  //   | FailedCast(t, HTyp.t, HTyp.t)
  //   | InvalidOperation(t, InvalidOperationError.t)
  };
};

let print_grain = (d: DHExp.t) => {
  //let s = "enum HazelSum<a, b> { L(a), R(b) }\n";
  let s = "import Hazel from \"hazel\"";
  let t = "Hazel.print_prog(result)";
  sprintf("%s\nlet result = %s\n%s", s, print_expression(d), t);
};
