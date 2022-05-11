exception FixF_Error;

let rec transform = (d: DHExp.t): IHExp.t => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let sigma = transform_var_map(sigma);
    EmptyHole(u, i, sigma);
  | NonEmptyHole(reason, u, i, sigma, d3) =>
    let sigma = transform_var_map(sigma);
    NonEmptyHole(reason, u, i, sigma, transform(d3));
  | Keyword(u, i, sigma, k) =>
    let sigma = transform_var_map(sigma);
    Keyword(u, i, sigma, k);
  | FreeVar(u, i, sigma, k) =>
    let sigma = transform_var_map(sigma);
    FreeVar(u, i, sigma, k);
  | InvalidText(u, i, sigma, text) =>
    let sigma = transform_var_map(sigma);
    InvalidText(u, i, sigma, text);
  | BoundVar(x) => BoundVar(x)
  | Let(Var(_), FixF(x, ty0, Lam(dp, _, d3)), dlet) =>
    LetRec(x, ty0, transform_pat(dp), transform(d3), transform(dlet))
  | Let(dp, d1, d2) =>
    Let(transform_pat(dp), transform(d1), transform(d2))
  | FixF(_) => raise(FixF_Error)
  | Lam(dp, ty, d3) => Lam(transform_pat(dp), ty, transform(d3))
  | Ap(d1, d2) => Ap(transform(d1), transform(d2))
  | ApBuiltin(ident, args) =>
    ApBuiltin(ident, List.rev(List.rev_map(transform, args)))
  | BoolLit(b) => BoolLit(b)
  | IntLit(i) => IntLit(i)
  | FloatLit(f) => FloatLit(f)
  | BinBoolOp(op, d1, d2) =>
    BinBoolOp(transform_bool_op(op), transform(d1), transform(d2))
  | BinIntOp(op, d1, d2) =>
    BinIntOp(transform_int_op(op), transform(d1), transform(d2))
  | BinFloatOp(op, d1, d2) =>
    BinFloatOp(transform_float_op(op), transform(d1), transform(d2))
  | ListNil(ty) => ListNil(ty)
  | Cons(d1, d2) => Cons(transform(d1), transform(d2))
  | Inj(ty, side, d1) => Inj(ty, side, transform(d1))
  | Pair(d1, d2) => Pair(transform(d1), transform(d2))
  | Triv => Triv
  | ConsistentCase(case) => ConsistentCase(transform_case(case))
  | InconsistentBranches(u, i, sigma, case) =>
    let sigma = transform_var_map(sigma);
    InconsistentBranches(u, i, sigma, transform_case(case));
  | Cast(d1, ty, ty') => Cast(transform(d1), ty, ty')
  | FailedCast(d1, ty, ty') => FailedCast(transform(d1), ty, ty')
  | InvalidOperation(d, err) => InvalidOperation(transform(d), err)
  };
}
and transform_bool_op = (op: DHExp.BinBoolOp.t): IHExp.bin_bool_op => {
  switch (op) {
  | And => And
  | Or => Or
  };
}
and transform_int_op = (op: DHExp.BinIntOp.t): IHExp.bin_int_op => {
  switch (op) {
  | Minus => Minus
  | Plus => Plus
  | Times => Times
  | Divide => Divide
  | LessThan => LessThan
  | GreaterThan => GreaterThan
  | Equals => Equals
  };
}
and transform_float_op = (op: DHExp.BinFloatOp.t): IHExp.bin_float_op => {
  switch (op) {
  | FMinus => FMinus
  | FPlus => FPlus
  | FTimes => FTimes
  | FDivide => FDivide
  | FLessThan => FLessThan
  | FGreaterThan => FGreaterThan
  | FEquals => FEquals
  };
}
and transform_case = (case: DHExp.case): IHExp.case => {
  switch (case) {
  | Case(scrut, rules, i) =>
    let rules = List.map(transform_rule, rules);
    Case(transform(scrut), rules, i);
  };
}
and transform_rule = (rule: DHExp.rule): IHExp.rule => {
  switch (rule) {
  | Rule(dp, d) => Rule(transform_pat(dp), transform(d))
  };
}
and transform_var_map = (dl: VarMap.t_(DHExp.t)) => {
  List.map(((x, d)) => (x, transform(d)), dl);
}
and transform_pat = (dp: DHPat.t): IHPat.t => {
  switch (dp) {
  | EmptyHole(u, i) => EmptyHole(u, i)
  | NonEmptyHole(reason, u, i, dp') =>
    NonEmptyHole(reason, u, i, transform_pat(dp'))
  | Wild => Wild
  | Keyword(u, i, k) => Keyword(u, i, k)
  | InvalidText(u, i, t) => InvalidText(u, i, t)
  | Var(x) => Var(x)
  | IntLit(i) => IntLit(i)
  | FloatLit(f) => FloatLit(f)
  | BoolLit(b) => BoolLit(b)
  | Inj(side, dp') => Inj(side, transform_pat(dp'))
  | ListNil => ListNil
  | Cons(dp1, dp2) => Cons(transform_pat(dp1), transform_pat(dp2))
  | Pair(dp1, dp2) => Pair(transform_pat(dp1), transform_pat(dp2))
  | Triv => Triv
  | Ap(dp1, dp2) => Ap(transform_pat(dp1), transform_pat(dp2))
  };
};
