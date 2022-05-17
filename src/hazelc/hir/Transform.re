exception FixFError;

let rec transform_exp = (d: DHExp.t): Hir.expr => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let (sigma, _) = transform_var_map(sigma);
    {expr_kind: EEmptyHole(u, i, sigma), expr_indet: true};

  | NonEmptyHole(reason, u, i, sigma, d3) =>
    let (sigma, _) = transform_var_map(sigma);
    let d3 = transform_exp(d3);
    {expr_kind: ENonEmptyHole(reason, u, i, sigma, d3), expr_indet: true};

  | Keyword(u, i, sigma, k) =>
    let (sigma, _) = transform_var_map(sigma);
    {expr_kind: EKeyword(u, i, sigma, k), expr_indet: true};

  | FreeVar(u, i, sigma, k) =>
    let (sigma, _) = transform_var_map(sigma);
    {expr_kind: EFreeVar(u, i, sigma, k), expr_indet: true};

  | InvalidText(u, i, sigma, text) =>
    let (sigma, _) = transform_var_map(sigma);
    {expr_kind: EInvalidText(u, i, sigma, text), expr_indet: true};

  // TODO: I think we need to lookup context here to determine indet or not?
  // Assume possibly indet for now just to be safe.
  | BoundVar(x) => {expr_kind: EBoundVar(x), expr_indet: true}

  | Let(Var(_), FixF(x, ty, Lam(dp, _, d3)), dlet) =>
    let dp = transform_pat(dp);
    let d3 = transform_exp(d3);
    let dlet = transform_exp(dlet);
    {
      expr_kind: ELetRec(x, ty, dp, d3, dlet),
      expr_indet: dp.pat_indet || d3.expr_indet || dlet.expr_indet,
    };

  | Let(dp, d1, d2) =>
    let dp = transform_pat(dp);
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    {
      expr_kind: ELet(dp, d1, d2),
      expr_indet: dp.pat_indet || d1.expr_indet || d2.expr_indet,
    };

  | FixF(_) => raise(FixFError)

  | Lam(dp, ty, d3) =>
    let dp = transform_pat(dp);
    let d3 = transform_exp(d3);
    {expr_kind: ELam(dp, ty, d3), expr_indet: dp.pat_indet || d3.expr_indet};

  | Ap(d1, d2) =>
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    {expr_kind: EAp(d1, d2), expr_indet: d1.expr_indet || d2.expr_indet};

  | ApBuiltin(ident, args) =>
    let args = args |> List.map(transform_exp);
    let args_indet = args |> List.exists((d: Hir.expr) => d.expr_indet);
    {expr_kind: EApBuiltin(ident, args), expr_indet: args_indet};

  | BoolLit(b) => {expr_kind: EBoolLit(b), expr_indet: false}

  | IntLit(i) => {expr_kind: EIntLit(i), expr_indet: false}

  | FloatLit(f) => {expr_kind: EFloatLit(f), expr_indet: false}

  | BinBoolOp(op, d1, d2) =>
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    let op = transform_bool_op(op);
    {
      expr_kind: EBinBoolOp(op, d1, d2),
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | BinIntOp(op, d1, d2) =>
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    let op = transform_int_op(op);
    {
      expr_kind: EBinIntOp(op, d1, d2),
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | BinFloatOp(op, d1, d2) =>
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    let op = transform_float_op(op);
    {
      expr_kind: EBinFloatOp(op, d1, d2),
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | ListNil(ty) => {expr_kind: EListNil(ty), expr_indet: false}

  | Cons(d1, d2) =>
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    {expr_kind: ECons(d1, d2), expr_indet: d1.expr_indet || d2.expr_indet};

  | Inj(ty, side, d1) =>
    let d1 = transform_exp(d1);
    {expr_kind: EInj(ty, side, d1), expr_indet: d1.expr_indet};

  | Pair(d1, d2) =>
    let d1 = transform_exp(d1);
    let d2 = transform_exp(d2);
    {expr_kind: EPair(d1, d2), expr_indet: d1.expr_indet || d2.expr_indet};

  | Triv => {expr_kind: ETriv, expr_indet: false}

  | ConsistentCase(case) =>
    // TODO: Auto false here?
    let case = transform_case(case);
    {expr_kind: EConsistentCase(case), expr_indet: false};

  | InconsistentBranches(u, i, sigma, case) =>
    let (sigma, _) = transform_var_map(sigma);
    let case = transform_case(case);
    {expr_kind: EInconsistentBranches(u, i, sigma, case), expr_indet: true};

  | Cast(d1, ty, ty') =>
    let d1 = transform_exp(d1);
    {expr_kind: ECast(d1, ty, ty'), expr_indet: true};

  | FailedCast(d1, ty, ty') =>
    let d1 = transform_exp(d1);
    {expr_kind: EFailedCast(d1, ty, ty'), expr_indet: true};

  | InvalidOperation(d1, err) =>
    let d1 = transform_exp(d1);
    {expr_kind: EInvalidOperation(d1, err), expr_indet: true};
  };
}

and transform_bool_op = (op: DHExp.BinBoolOp.t): Hir.bin_bool_op => {
  switch (op) {
  | And => Hir.OpAnd
  | Or => Hir.OpOr
  };
}

and transform_int_op = (op: DHExp.BinIntOp.t): Hir.bin_int_op => {
  switch (op) {
  | Minus => Hir.OpMinus
  | Plus => Hir.OpPlus
  | Times => Hir.OpTimes
  | Divide => Hir.OpDivide
  | LessThan => Hir.OpLessThan
  | GreaterThan => Hir.OpGreaterThan
  | Equals => Hir.OpEquals
  };
}

and transform_float_op = (op: DHExp.BinFloatOp.t): Hir.bin_float_op => {
  switch (op) {
  | FMinus => Hir.OpFMinus
  | FPlus => Hir.OpFPlus
  | FTimes => Hir.OpFTimes
  | FDivide => Hir.OpFDivide
  | FLessThan => Hir.OpFLessThan
  | FGreaterThan => Hir.OpFGreaterThan
  | FEquals => Hir.OpFEquals
  };
}

and transform_case = (case: DHExp.case): Hir.case => {
  switch (case) {
  | Case(scrut, rules, i) =>
    let scrut = transform_exp(scrut);
    let rules = rules |> List.map(transform_rule);
    let rules_indet =
      rules |> List.exists((rule: Hir.rule) => rule.rule_indet);
    {
      case_kind: ECase(scrut, rules, i),
      case_indet: scrut.expr_indet || rules_indet,
    };
  };
}

and transform_rule = (rule: DHExp.rule): Hir.rule => {
  switch (rule) {
  | Rule(dp, d) =>
    let dp = transform_pat(dp);
    let d = transform_exp(d);
    {rule_kind: ERule(dp, d), rule_indet: dp.pat_indet || d.expr_indet};
  };
}

and transform_var_map =
    (sigma: VarMap.t_(DHExp.t)): (VarMap.t_(Hir.expr), Hir.has_indet) => {
  let sigma = sigma |> List.map(((x, d)) => (x, transform_exp(d)));
  let sigma_indet =
    sigma |> List.exists(((_, d): (Var.t, Hir.expr)) => d.expr_indet);
  (sigma, sigma_indet);
}

and transform_pat = (dp: DHPat.t): Hir.pat => {
  switch (dp) {
  | EmptyHole(u, i) => {pat_kind: PEmptyHole(u, i), pat_indet: true}

  | NonEmptyHole(reason, u, i, dp') =>
    let dp' = transform_pat(dp');
    {pat_kind: PNonEmptyHole(reason, u, i, dp'), pat_indet: true};

  | Keyword(u, i, k) => {pat_kind: PKeyword(u, i, k), pat_indet: true}

  | InvalidText(u, i, t) => {
      pat_kind: PInvalidText(u, i, t),
      pat_indet: true,
    }

  | Wild => {pat_kind: PWild, pat_indet: false}

  | Var(x) => {pat_kind: PVar(x), pat_indet: false}

  | IntLit(i) => {pat_kind: PIntLit(i), pat_indet: false}

  | FloatLit(f) => {pat_kind: PFloatLit(f), pat_indet: false}

  | BoolLit(b) => {pat_kind: PBoolLit(b), pat_indet: false}

  | Inj(side, dp') =>
    let dp' = transform_pat(dp');
    {pat_kind: PInj(side, dp'), pat_indet: dp'.pat_indet};

  | ListNil => {pat_kind: PListNil, pat_indet: false}

  | Cons(dp1, dp2) =>
    let dp1 = transform_pat(dp1);
    let dp2 = transform_pat(dp2);
    {pat_kind: PCons(dp1, dp2), pat_indet: dp1.pat_indet || dp2.pat_indet};

  | Pair(dp1, dp2) =>
    let dp1 = transform_pat(dp1);
    let dp2 = transform_pat(dp2);
    {pat_kind: PPair(dp1, dp2), pat_indet: dp1.pat_indet || dp2.pat_indet};

  | Triv => {pat_kind: PTriv, pat_indet: false}

  | Ap(dp1, dp2) =>
    let dp1 = transform_pat(dp1);
    let dp2 = transform_pat(dp2);
    {pat_kind: PAp(dp1, dp2), pat_indet: dp1.pat_indet || dp2.pat_indet};
  };
};

let transform = (d: DHExp.t): Hir.expr => transform_exp(d);
