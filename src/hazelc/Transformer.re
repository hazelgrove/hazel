exception FixF_Error;

let rec transform = (d: DHExp.t) => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let sigma = transform_var_map(sigma);
    IHExp.EmptyHole(u, i, sigma);
  | NonEmptyHole(reason, u, i, sigma, d3) =>
    let sigma = transform_var_map(sigma);
    IHExp.NonEmptyHole(reason, u, i, sigma, transform(d3));
  | Keyword(u, i, sigma, k) =>
    let sigma = transform_var_map(sigma);
    IHExp.Keyword(u, i, sigma, k);
  | FreeVar(u, i, sigma, k) =>
    let sigma = transform_var_map(sigma);
    IHExp.FreeVar(u, i, sigma, k);
  | InvalidText(u, i, sigma, text) =>
    let sigma = transform_var_map(sigma);
    IHExp.InvalidText(u, i, sigma, text);
  | BoundVar(x) => IHExp.BoundVar(x)
  | Let(dp, d1, d2) =>
    IHExp.Let(transform_pat(dp), transform(d1), transform(d2))
  | FixF(x, ty0, Lam(dp, _, d3)) =>
    IHExp.RecFun(x, ty0, transform_pat(dp), transform(d3))
  | FixF(_) => raise(FixF_Error)
  | Lam(dp, ty, d3) => IHExp.Lam(transform_pat(dp), ty, transform(d3))
  | Ap(d1, d2) => IHExp.Ap(transform(d1), transform(d2))
  | BoolLit(b) => IHExp.BoolLit(b)
  | IntLit(i) => IHExp.IntLit(i)
  | FloatLit(f) => IHExp.FloatLit(f)
  | BinBoolOp(op, d1, d2) =>
    IHExp.BinBoolOp(transform_bool_op(op), transform(d1), transform(d2))
  | BinIntOp(op, d1, d2) =>
    IHExp.BinIntOp(transform_int_op(op), transform(d1), transform(d2))
  | BinFloatOp(op, d1, d2) =>
    IHExp.BinFloatOp(transform_float_op(op), transform(d1), transform(d2))
  | ListNil(ty) => IHExp.ListNil(ty)
  | Cons(d1, d2) => IHExp.Cons(transform(d1), transform(d2))
  | Inj(ty, side, d1) => IHExp.Inj(ty, side, transform(d1))
  | Pair(d1, d2) => IHExp.Pair(transform(d1), transform(d2))
  | Triv => IHExp.Triv
  | ConsistentCase(case) => IHExp.ConsistentCase(transform_case(case))
  | InconsistentBranches(u, i, sigma, case) =>
    let sigma = transform_var_map(sigma);
    IHExp.InconsistentBranches(u, i, sigma, transform_case(case));
  | Cast(d1, ty, ty') => IHExp.Cast(transform(d1), ty, ty')
  | FailedCast(d1, ty, ty') => IHExp.FailedCast(transform(d1), ty, ty')
  | InvalidOperation(d, err) => IHExp.InvalidOperation(transform(d), err)
  };
}
and transform_bool_op = (op: DHExp.BinBoolOp.t) => {
  switch (op) {
  | And => IHExp.BinBoolOp.And
  | Or => IHExp.BinBoolOp.Or
  };
}
and transform_int_op = (op: DHExp.BinIntOp.t) => {
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
and transform_float_op = (op: DHExp.BinFloatOp.t) => {
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
and transform_case = (case: DHExp.case) => {
  switch (case) {
  | Case(scrut, rules, i) =>
    let rules = List.map(transform_rule, rules);
    IHExp.Case(transform(scrut), rules, i);
  };
}
and transform_rule = (rule: DHExp.rule) => {
  switch (rule) {
  | Rule(dp, d) => IHExp.Rule(transform_pat(dp), transform(d))
  };
}
and transform_var_map = (dl: VarMap.t_(DHExp.t)) => {
  List.map(((x, d)) => (x, transform(d)), dl);
}
and transform_pat = (dp: DHPat.t): IHPat.t => {
  switch (dp) {
  | EmptyHole(u, i) => IHPat.EmptyHole(u, i)
  | NonEmptyHole(reason, u, i, dp') =>
    IHPat.NonEmptyHole(reason, u, i, transform_pat(dp'))
  | Wild => IHPat.Wild
  | Keyword(u, i, k) => IHPat.Keyword(u, i, k)
  | InvalidText(u, i, t) => IHPat.InvalidText(u, i, t)
  | Var(x) => IHPat.Var(x)
  | IntLit(i) => IHPat.IntLit(i)
  | FloatLit(f) => IHPat.FloatLit(f)
  | BoolLit(b) => IHPat.BoolLit(b)
  | Inj(side, dp') => IHPat.Inj(side, transform_pat(dp'))
  | ListNil => IHPat.ListNil
  | Cons(dp1, dp2) => IHPat.Cons(transform_pat(dp1), transform_pat(dp2))
  | Pair(dp1, dp2) => IHPat.Pair(transform_pat(dp1), transform_pat(dp2))
  | Triv => IHPat.Triv
  | Ap(dp1, dp2) => IHPat.Ap(transform_pat(dp1), transform_pat(dp2))
  };
};
