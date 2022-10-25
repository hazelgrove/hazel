//fill hole u with d in d0
module FillResumeState = {
  type t = {
    metavar: MetaVar.t,
    d_inserted: DHExp.t,
  };

  let mk = (metavar: MetaVar.t, d_inserted: DHExp.t): t => {
    {metavar, d_inserted};
  };
};

let rec fill = (s: FillResumeState.t, d_prev_result: DHExp.t): DHExp.t => {
  let filld = fill(s);
  let fills = fill_env(s);
  switch (d_prev_result) {
  | EmptyHole(v, _)
  | NonEmptyHole(_, v, _, _)
  | ExpandingKeyword(v, _, _)
  | FreeVar(v, _, _)
  | InvalidText(v, _, _) =>
    MetaVar.eq(v, s.metavar) ? s.d_inserted : d_prev_result
  | InconsistentBranches(v, i, c) =>
    if (MetaVar.eq(v, s.metavar)) {
      s.d_inserted;
    } else {
      InconsistentBranches(v, i, c |> fill_case(s));
    }
  | ListLit(v, i, err, ty, ld) =>
    MetaVar.eq(v, s.metavar)
      ? s.d_inserted : ListLit(v, i, err, ty, ld |> List.map(filld))

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
  | ConsistentCase(c) => ConsistentCase(c |> fill_case(s))
  | Cast(d1, ty1, ty2) => Cast(d1 |> filld, ty1, ty2)
  | FailedCast(d1, ty1, ty2) => FailedCast(d1 |> filld, ty1, ty2)
  | InvalidOperation(d1, _) => d1 |> filld

  | BoundVar(_)
  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Tag(_) => d_prev_result
  };
}
and fill_env =
    (s: FillResumeState.t, c: ClosureEnvironment.t): ClosureEnvironment.t => {
  let (c', _) =
    ClosureEnvironment.map(
      ((_, d0)) => fill(s, d0),
      c,
      EnvironmentIdGen.init,
    );
  c';
}
and fill_case = (s: FillResumeState.t, c: DHExp.case): DHExp.case => {
  //TODO: pattern
  let filld = fill(s);
  switch (c) {
  | Case(d1, rules, _) =>
    Case(
      d1 |> filld,
      rules |> List.map((DHExp.Rule(dp, d2)) => DHExp.Rule(dp, d2 |> filld)),
      0,
    )
  };
};

let diff_of_DHExp =
    (d_prev: DHExp.t, d_now: DHExp.t): option(FillResumeState.t) => {
  switch (d_prev, d_now) {
  | (EmptyHole(v, _), _) => Some(FillResumeState.mk(v, d_now))
  | _ => None
  };
};

let fill_resume_evaluate =
    (
      env: Environment.t,
      d_now: DHExp.t,
      d_prev: option(DHExp.t),
      d_prev_result: option(DHExp.t),
    )
    : (EvaluatorState.t, EvaluatorResult.t) => {
  let state =
    switch (d_prev) {
    | Some(d_prev') => diff_of_DHExp(d_prev', d_now)
    | _ => None
    };
  let fill_result =
    switch (state, d_prev_result) {
    | (Some(s), Some(d_prev_result')) => fill(s, d_prev_result')
    | _ => d_now
    };
  Evaluator.evaluate(env, fill_result);
};
