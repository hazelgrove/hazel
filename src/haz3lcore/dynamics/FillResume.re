//fill hole u with d in d0
let rec fill = (u: MetaVar.t, d: DHExp.t, d0: DHExp.t): DHExp.t => {
  let filld = fill(u, d);
  let fills = fill_env(u, d);
  switch (d0) {
  | EmptyHole(v, _)
  | NonEmptyHole(_, v, _, _)
  | ExpandingKeyword(v, _, _)
  | FreeVar(v, _, _)
  | InvalidText(v, _, _) => MetaVar.eq(v, u) ? d : d0
  | InconsistentBranches(v, i, c) =>
    if (MetaVar.eq(v, u)) {
      d;
    } else {
      InconsistentBranches(v, i, c |> fill_case(u, d));
    }
  | ListLit(v, i, err, ty, ld) =>
    MetaVar.eq(v, u) ? d : ListLit(v, i, err, ty, ld |> List.map(filld))

  | Closure(env, d1) => Closure(env |> fills, d1 |> filld)

  | Sequence(d1, d2) => Sequence(d1 |> filld, d2 |> filld)
  | Let(pat, d1, d2) => Let(pat, d1, d2 |> filld) //TODO: not correct
  | FixF(var, ty, d1) => FixF(var, ty, d1 |> filld)
  | Fun(pat, ty, d1) => Fun(pat, ty, d1 |> filld)
  | Ap(d1, d2) => Ap(d1 |> filld, d2 |> filld)
  | ApBuiltin(func_name, args) =>
    ApBuiltin(func_name, List.map(filld, args))
  | BinBoolOp(op, d1, d2) => BinBoolOp(op, d1 |> filld, d2 |> filld)
  | BinIntOp(op, d1, d2) => BinIntOp(op, d1 |> filld, d2 |> filld)
  | BinFloatOp(op, d1, d2) => BinFloatOp(op, d1 |> filld, d2 |> filld)
  | BinStringOp(op, d1, d2) => BinStringOp(op, d1 |> filld, d2 |> filld)
  | Cons(d1, d2) => Cons(d1 |> filld, d2 |> filld)
  | Tuple(ld) => Tuple(ld |> List.map(filld))
  | Prj(d1, i) => Prj(d1 |> filld, i)
  | Inj(ty, side, d1) => Inj(ty, side, d1 |> filld)
  | ConsistentCase(c) => ConsistentCase(c |> fill_case(u, d))
  | Cast(d1, ty1, ty2) => Cast(d1 |> filld, ty1, ty2)
  | FailedCast(d1, ty1, ty2) => FailedCast(d1 |> filld, ty1, ty2)
  | InvalidOperation(d1, _) => d1 |> filld

  | BoundVar(_)
  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Tag(_) => d0
  };
}
and fill_env =
    (u: MetaVar.t, d: DHExp.t, c: ClosureEnvironment.t): ClosureEnvironment.t => {
  let (c', _) =
    ClosureEnvironment.map(
      ((_, d0)) => fill(u, d, d0),
      c,
      EnvironmentIdGen.init,
    );
  c';
}
and fill_case = (u: MetaVar.t, d: DHExp.t, c: DHExp.case): DHExp.case => {
  //TODO: pattern
  let filld = fill(u, d);
  switch (c) {
  | Case(d1, rules, _) =>
    Case(
      d1 |> filld,
      rules |> List.map((DHExp.Rule(dp, d2)) => DHExp.Rule(dp, d2 |> filld)),
      0,
    )
  };
};
