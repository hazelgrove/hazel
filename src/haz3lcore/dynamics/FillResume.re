//fill hole u with d in d0
module FillResumeState = {
  [@deriving (show({with_path: false}), sexp, yojson)]
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
  | Cast(Closure(_, EmptyHole(v, _)), _, _)
  | Closure(_, EmptyHole(v, _))
  | Cast(Closure(_, NonEmptyHole(_, v, _, _)), _, _)
  | Closure(_, NonEmptyHole(_, v, _, _))
  | Cast(Closure(_, ExpandingKeyword(v, _, _)), _, _)
  | Closure(_, ExpandingKeyword(v, _, _))
  | Cast(Closure(_, FreeVar(v, _, _)), _, _)
  | Closure(_, FreeVar(v, _, _))
  | Cast(Closure(_, InvalidText(v, _, _)), _, _)
  | Closure(_, InvalidText(v, _, _)) =>
    MetaVar.eq(v, s.metavar) ? s.d_inserted : d_prev_result
  | Cast(Closure(env, InconsistentBranches(v, i, c)), ty1, ty2) =>
    if (MetaVar.eq(v, s.metavar)) {
      s.d_inserted;
    } else {
      Cast(
        Closure(env, InconsistentBranches(v, i, c |> fill_case(s))),
        ty1,
        ty2,
      );
    }
  | Closure(env, InconsistentBranches(v, i, c)) =>
    if (MetaVar.eq(v, s.metavar)) {
      s.d_inserted;
    } else {
      Closure(env, InconsistentBranches(v, i, c |> fill_case(s)));
    }

  | Closure(env, d1) => Closure(env |> fills, d1 |> filld)
  | ListLit(v, i, err, ty, ld) =>
    MetaVar.eq(v, s.metavar)
      ? s.d_inserted : ListLit(v, i, err, ty, ld |> List.map(filld))
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
  | EmptyHole(_)
  | NonEmptyHole(_)
  | ExpandingKeyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | InconsistentBranches(_)
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

let one_some = (d1: option('a), d2: option('a)): option('a) => {
  switch (d1, d2) {
  | (Some(_), Some(_)) => None
  | (Some(_), None) => d1
  | (None, Some(_)) => d2
  | (None, None) => None
  };
};

let rec diff_of_DHExp =
        (d_prev: DHExp.t, d_now: DHExp.t): option(FillResumeState.t) => {
  switch (d_prev, d_now) {
  | (dp, dn) when dp == dn => None
  | (_, EmptyHole(_)) => None
  | (EmptyHole(v, _), _)
  | (NonEmptyHole(_, v, _, _), _)
  | (ExpandingKeyword(v, _, _), _)
  | (FreeVar(v, _, _), _)
  | (InvalidText(v, _, _), _)
  | (InconsistentBranches(v, _, _), _)
  | (ListLit(v, _, _, _, _), _) => Some(FillResumeState.mk(v, d_now))
  | (Closure(_, d_closure), _) => diff_of_DHExp(d_closure, d_now) //TODO: not sure
  | (Cast(d1p, _, _), _) => diff_of_DHExp(d1p, d_now)
  | (Sequence(d1p, d2p), Sequence(d1n, d2n))
  | (Ap(d1p, d2p), Ap(d1n, d2n))
  | (Cons(d1p, d2p), Cons(d1n, d2n)) =>
    one_some(diff_of_DHExp(d1p, d1n), diff_of_DHExp(d2p, d2n))
  | (ApBuiltin(sp, d1ps), ApBuiltin(sn, d1ns)) when sp == sn =>
    List.map2(diff_of_DHExp, d1ps, d1ns) |> List.fold_left(one_some, None)
  | (Tuple(d1ps), Tuple(d1ns)) =>
    List.map2(diff_of_DHExp, d1ps, d1ns) |> List.fold_left(one_some, None)
  | (Let(dpp, d1p, d2p), Let(dpn, d1n, d2n)) when dpp == dpn =>
    one_some(diff_of_DHExp(d1p, d1n), diff_of_DHExp(d2p, d2n))
  | (FixF(sp, typ, d1p), FixF(sn, tyn, d1n))
      when sp == sn && Typ.eq(typ, tyn) =>
    diff_of_DHExp(d1p, d1n)
  | (Fun(dpp, typ, d1p), Fun(dpn, tyn, d1n))
      when dpp == dpn && Typ.eq(typ, tyn) =>
    diff_of_DHExp(d1p, d1n)
  | (Inj(typ, sidep, d1p), Inj(tyn, siden, d1n))
      when sidep == siden && Typ.eq(typ, tyn) =>
    diff_of_DHExp(d1p, d1n)
  | (Prj(d1p, intp), Prj(d1n, intn)) when intp == intn =>
    diff_of_DHExp(d1p, d1n)
  | (BinBoolOp(opp, d1p, d2p), BinBoolOp(opn, d1n, d2n)) when opp == opn =>
    one_some(diff_of_DHExp(d1p, d1n), diff_of_DHExp(d2p, d2n))
  | (BinIntOp(opp, d1p, d2p), BinIntOp(opn, d1n, d2n)) when opp == opn =>
    one_some(diff_of_DHExp(d1p, d1n), diff_of_DHExp(d2p, d2n))
  | (BinFloatOp(opp, d1p, d2p), BinFloatOp(opn, d1n, d2n)) when opp == opn =>
    one_some(diff_of_DHExp(d1p, d1n), diff_of_DHExp(d2p, d2n))
  | (BinStringOp(opp, d1p, d2p), BinStringOp(opn, d1n, d2n)) when opp == opn =>
    one_some(diff_of_DHExp(d1p, d1n), diff_of_DHExp(d2p, d2n))
  | (FailedCast(d1p, ty1p, ty2p), FailedCast(d1n, ty1n, ty2n))
      when Typ.eq(ty1p, ty1n) && Typ.eq(ty2p, ty2n) =>
    diff_of_DHExp(d1p, d1n)
  | (InvalidOperation(d1p, errp), InvalidOperation(d1n, errn))
      when errp == errn =>
    diff_of_DHExp(d1p, d1n)
  | (TestLit(_), _)
  | (BoolLit(_), _)
  | (IntLit(_), _)
  | (FloatLit(_), _)
  | (StringLit(_), _)
  | (BoundVar(_), _)
  | (Tag(_), _) => None
  | _ => None
  };
}
and diff_of_case =
    (c_prev: DHExp.case, c_now: DHExp.case): option(FillResumeState.t) => {
  switch (c_prev, c_now) {
  | (Case(d1p, rulesp, _), Case(d1n, rulesn, _)) =>
    let diff_d1 = diff_of_DHExp(d1p, d1n);
    let diff_rules =
      List.map2(diff_of_rule, rulesp, rulesn)
      |> List.fold_left(one_some, None);
    one_some(diff_d1, diff_rules);
  };
}
and diff_of_rule =
    (r_prev: DHExp.rule, r_now: DHExp.rule): option(FillResumeState.t) => {
  switch (r_prev, r_now) {
  | (Rule(dpp, d1p), Rule(dpn, d1n)) when dpp == dpn =>
    diff_of_DHExp(d1p, d1n)
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
  // let _ = (d_prev, d_prev_result);
  // Evaluator.evaluate(env, d_now);
};
