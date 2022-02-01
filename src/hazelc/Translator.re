let rec translate = (d: DHExp.t) => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let sigma = translate_var_map(sigma);
    IHExp.EmptyHole(u, i, sigma);
  | NonEmptyHole(reason, u, i, sigma, d3) =>
    let sigma = translate_var_map(sigma);
    IHExp.NonEmptyHole(reason, u, i, sigma, translate(d3));
  | Keyword(u, i, sigma, k) =>
    let sigma = translate_var_map(sigma);
    IHExp.Keyword(u, i, sigma, k);
  | FreeVar(u, i, sigma, k) =>
    let sigma = translate_var_map(sigma);
    IHExp.FreeVar(u, i, sigma, k);
  | InvalidText(u, i, sigma, text) =>
    let sigma = translate_var_map(sigma);
    IHExp.InvalidText(u, i, sigma, text);
  | BoundVar(x) => IHExp.BoundVar(x)
  | Let(dp, d1, d2) =>
    IHExp.Let(translate_pat(dp), translate(d1), translate(d2))
  | FixF(x, ty, d1) => IHExp.FixF(x, ty, translate(d1))
  | Lam(dp, ty, d3) => IHExp.Lam(translate_pat(dp), ty, translate(d3))
  | Ap(d1, d2) => IHExp.Ap(translate(d1), translate(d2))
  | BoolLit(b) => IHExp.BoolLit(b)
  | IntLit(i) => IHExp.IntLit(i)
  | FloatLit(f) => IHExp.FloatLit(f)
  | BinBoolOp(op, d1, d2) =>
    IHExp.BinBoolOp(translate_bool_op(op), translate(d1), translate(d2))
  | BinIntOp(op, d1, d2) =>
    IHExp.BinIntOp(translate_int_op(op), translate(d1), translate(d2))
  | BinFloatOp(op, d1, d2) =>
    IHExp.BinFloatOp(translate_float_op(op), translate(d1), translate(d2))
  | ListNil(ty) => IHExp.ListNil(ty)
  | Cons(d1, d2) => IHExp.Cons(translate(d1), translate(d2))
  | Inj(ty, side, d1) => IHExp.Inj(ty, side, translate(d1))
  | Pair(d1, d2) => IHExp.Pair(translate(d1), translate(d2))
  | Triv => IHExp.Triv
  | ConsistentCase(case) => IHExp.ConsistentCase(translate_case(case))
  | InconsistentBranches(u, i, sigma, case) =>
    let sigma = translate_var_map(sigma);
    IHExp.InconsistentBranches(u, i, sigma, translate_case(case));
  | Cast(d1, ty, ty') => IHExp.Cast(translate(d1), ty, ty')
  | FailedCast(d1, ty, ty') => IHExp.FailedCast(translate(d1), ty, ty')
  | InvalidOperation(d, err) => IHExp.InvalidOperation(translate(d), err)
  };
}
and translate_bool_op = (op: DHExp.BinBoolOp.t) => {
  switch (op) {
  | And => IHExp.BinBoolOp.And
  | Or => IHExp.BinBoolOp.Or
  };
}
and translate_int_op = (op: DHExp.BinIntOp.t) => {
  switch (op) {
  | Minus => IHExp.BinIntOp.Minus
  | Plus => IHExp.BinIntOp.Plus
  | Times => IHExp.BinIntOp.Times
  | Divide => IHExp.BinIntOp.Divide
  | LessThan => IHExp.BinIntOp.LessThan
  | GreaterThan => IHExp.BinIntOp.GreaterThan
  | Equals => IHExp.BinIntOp.Equals
  };
}
and translate_float_op = (op: DHExp.BinFloatOp.t) => {
  switch (op) {
  | FMinus => IHExp.BinFloatOp.FMinus
  | FPlus => IHExp.BinFloatOp.FPlus
  | FTimes => IHExp.BinFloatOp.FTimes
  | FDivide => IHExp.BinFloatOp.FDivide
  | FLessThan => IHExp.BinFloatOp.FLessThan
  | FGreaterThan => IHExp.BinFloatOp.FGreaterThan
  | FEquals => IHExp.BinFloatOp.FEquals
  };
}
and translate_case = (case: DHExp.case) => {
  switch (case) {
  | Case(scrut, rules, i) =>
    let rules = List.map(translate_rule, rules);
    IHExp.Case(translate(scrut), rules, i);
  };
}
and translate_rule = (rule: DHExp.rule) => {
  switch (rule) {
  | Rule(dp, d) => IHExp.Rule(translate_pat(dp), translate(d))
  };
}
and translate_var_map = (dl: VarMap.t_(DHExp.t)) => {
  List.map(((x, d)) => (x, translate(d)), dl);
}
and translate_pat = (dp: DHPat.t): IHPat.t => {
  switch (dp) {
  | EmptyHole(u, i) => IHPat.EmptyHole(u, i)
  | NonEmptyHole(reason, u, i, dp') =>
    IHPat.NonEmptyHole(reason, u, i, translate_pat(dp'))
  | Wild => IHPat.Wild
  | Keyword(u, i, k) => IHPat.Keyword(u, i, k)
  | InvalidText(u, i, t) => IHPat.InvalidText(u, i, t)
  | Var(x) => IHPat.Var(x)
  | IntLit(i) => IHPat.IntLit(i)
  | FloatLit(f) => IHPat.FloatLit(f)
  | BoolLit(b) => IHPat.BoolLit(b)
  | Inj(side, dp') => IHPat.Inj(side, translate_pat(dp'))
  | ListNil => IHPat.ListNil
  | Cons(dp1, dp2) => IHPat.Cons(translate_pat(dp1), translate_pat(dp2))
  | Pair(dp1, dp2) => IHPat.Pair(translate_pat(dp1), translate_pat(dp2))
  | Triv => IHPat.Triv
  | Ap(dp1, dp2) => IHPat.Ap(translate_pat(dp1), translate_pat(dp2))
  };
};
