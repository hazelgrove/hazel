exception FixFError;
exception FreeVarError;
exception WrongTypeError;

/*
   Througout the transformation process, we maintain a context mapping
   variables to types and the presence of indetermine subexpressions.
 */
[@deriving sexp]
type context = VarMap.t_((HTyp.t, Hir.has_indet));

let rec transform_exp = (ctx: context, d: DHExp.t): Hir.expr => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let (sigma, _) = transform_var_map(ctx, sigma);
    {expr_kind: EEmptyHole(u, i, sigma), expr_ty: Hole, expr_indet: true};

  | NonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, _) = transform_var_map(ctx, sigma);
    let d3 = transform_exp(ctx, d');
    {
      expr_kind: ENonEmptyHole(reason, u, i, sigma, d3),
      expr_ty: Hole,
      expr_indet: true,
    };

  | Keyword(u, i, sigma, k) =>
    let (sigma, _) = transform_var_map(ctx, sigma);
    {expr_kind: EKeyword(u, i, sigma, k), expr_ty: Hole, expr_indet: true};

  | FreeVar(u, i, sigma, k) =>
    let (sigma, _) = transform_var_map(ctx, sigma);
    {expr_kind: EFreeVar(u, i, sigma, k), expr_ty: Hole, expr_indet: true};

  | InvalidText(u, i, sigma, text) =>
    let (sigma, _) = transform_var_map(ctx, sigma);
    {
      expr_kind: EInvalidText(u, i, sigma, text),
      expr_ty: Hole,
      expr_indet: true,
    };

  | BoundVar(x) =>
    switch (VarMap.lookup(ctx, x)) {
    | Some((expr_ty, expr_indet)) => {
        expr_kind: EBoundVar(x),
        expr_ty,
        expr_indet,
      }
    // TODO: Not sure what behavior is needed here.
    | None => raise(FreeVarError)
    }

  | Let(Var(_), FixF(x, ty, Lam(dp, _, d3)), body) =>
    // TODO: Not really sure if any of this recursive function handling is right...
    let (dp, ctx') = transform_pat(ctx, dp, ty, true);
    let d3 = transform_exp(ctx', d3);
    let body = transform_exp(ctx', body);
    {
      expr_kind: ELetRec(x, ty, dp, d3, body),
      expr_ty: ty,
      expr_indet: dp.pat_indet || d3.expr_indet || body.expr_indet,
    };

  | Let(dp, d1, d2) =>
    let d1 = transform_exp(ctx, d1);
    let (dp, body_ctx) = transform_pat(ctx, dp, d1.expr_ty, d1.expr_indet);
    let d2 = transform_exp(body_ctx, d2);
    {
      expr_kind: ELet(dp, d1, d2),
      expr_ty: d2.expr_ty,
      expr_indet: dp.pat_indet || d1.expr_indet || d2.expr_indet,
    };

  | FixF(_) => raise(FixFError)

  | Lam(dp, dp_ty, body) =>
    // TODO: Can't assume anything about indet-ness of argument when called?
    let (dp, body_ctx) = transform_pat(ctx, dp, dp_ty, true);
    let body = transform_exp(body_ctx, body);
    {
      expr_kind: ELam(dp, dp_ty, body),
      expr_ty: Arrow(dp_ty, body.expr_ty),
      expr_indet: dp.pat_indet || body.expr_indet,
    };

  | Ap(fn, arg) =>
    let fn = transform_exp(ctx, fn);
    let arg = transform_exp(ctx, arg);
    switch (fn.expr_ty) {
    | Arrow(ty, ty') when HTyp.eq(ty, arg.expr_ty) => {
        expr_kind: EAp(fn, arg),
        expr_ty: ty',
        expr_indet: fn.expr_indet || arg.expr_indet,
      }
    | _ => raise(WrongTypeError)
    };

  | ApBuiltin(name, args) =>
    let args = args |> List.map(transform_exp(ctx));
    let args_indet = args |> List.exists((d: Hir.expr) => d.expr_indet);

    switch (VarMap.lookup(ctx, name)) {
    // TODO: Further verification of looked-up type.
    | Some((Arrow(_, ty2), fn_indet)) => {
        expr_kind: EApBuiltin(name, args),
        expr_ty: ty2,
        expr_indet: fn_indet || args_indet,
      }
    | _ => raise(WrongTypeError)
    };

  | BoolLit(b) => {expr_kind: EBoolLit(b), expr_ty: Bool, expr_indet: false}

  | IntLit(i) => {expr_kind: EIntLit(i), expr_ty: Int, expr_indet: false}

  | FloatLit(f) => {
      expr_kind: EFloatLit(f),
      expr_ty: Float,
      expr_indet: false,
    }

  // TODO: Check to ensure types of d1 and d2 are correct?
  | BinBoolOp(op, d1, d2) =>
    let d1 = transform_exp(ctx, d1);
    let d2 = transform_exp(ctx, d2);
    let op = transform_bool_op(op);
    {
      expr_kind: EBinBoolOp(op, d1, d2),
      expr_ty: Bool,
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | BinIntOp(op, d1, d2) =>
    let d1 = transform_exp(ctx, d1);
    let d2 = transform_exp(ctx, d2);
    let op = transform_int_op(op);
    {
      expr_kind: EBinIntOp(op, d1, d2),
      expr_ty: Int,
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | BinFloatOp(op, d1, d2) =>
    let d1 = transform_exp(ctx, d1);
    let d2 = transform_exp(ctx, d2);
    let op = transform_float_op(op);
    {
      expr_kind: EBinFloatOp(op, d1, d2),
      expr_ty: Float,
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | ListNil(ty) => {expr_kind: EListNil(ty), expr_ty: ty, expr_indet: false}

  // TODO: Check types of d1 and d2 are the same?
  | Cons(d1, d2) =>
    let d1 = transform_exp(ctx, d1);
    let d2 = transform_exp(ctx, d2);
    {
      expr_kind: ECons(d1, d2),
      expr_ty: List(d1.expr_ty),
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | Inj(ty, side, d1) =>
    let d1 = transform_exp(ctx, d1);
    {expr_kind: EInj(ty, side, d1), expr_ty: ty, expr_indet: d1.expr_indet};

  | Pair(d1, d2) =>
    let d1 = transform_exp(ctx, d1);
    let d2 = transform_exp(ctx, d2);
    {
      expr_kind: EPair(d1, d2),
      expr_ty: Prod([d1.expr_ty, d2.expr_ty]),
      expr_indet: d1.expr_indet || d2.expr_indet,
    };

  | Triv => {expr_kind: ETriv, expr_ty: Prod([]), expr_indet: false}

  // TODO: Check to ensure synthesized case type is not Hole.
  | ConsistentCase(case) =>
    let case = transform_case(ctx, case);
    {
      expr_kind: EConsistentCase(case),
      expr_ty: case.case_ty,
      // TODO: Not sure if this is automatically false.
      expr_indet: false,
    };

  // TODO: Check to ensure synthesized case type is Hole.
  | InconsistentBranches(u, i, sigma, case) =>
    let (sigma, _) = transform_var_map(ctx, sigma);
    let case = transform_case(ctx, case);
    {
      expr_kind: EInconsistentBranches(u, i, sigma, case),
      expr_ty: Hole,
      expr_indet: true,
    };

  // TODO: Check that ty and ty' are consistent.
  | Cast(d1, ty, ty') =>
    let d1 = transform_exp(ctx, d1);
    {expr_kind: ECast(d1, ty, ty'), expr_ty: ty', expr_indet: true};

  // TODO: Check types.
  | FailedCast(d1, ty, ty') =>
    let d1 = transform_exp(ctx, d1);
    {expr_kind: EFailedCast(d1, ty, ty'), expr_ty: ty', expr_indet: true};

  | InvalidOperation(d1, err) =>
    let d1 = transform_exp(ctx, d1);
    {
      expr_kind: EInvalidOperation(d1, err),
      expr_ty: d1.expr_ty,
      expr_indet: true,
    };
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

and transform_case = (ctx: context, case: DHExp.case): Hir.case => {
  switch (case) {
  // TODO: Check that all rules have same type.
  | Case(scrut, rules, i) =>
    let scrut = transform_exp(ctx, scrut);
    let rules =
      rules
      |> List.map(rule =>
           transform_rule(ctx, rule, scrut.expr_ty, scrut.expr_indet)
         );
    let rules_indet =
      rules |> List.exists((rule: Hir.rule) => rule.rule_indet);

    // TODO: What if no rules?
    let ty = List.hd(rules).rule_ty;
    {
      case_kind: ECase(scrut, rules, i),
      case_ty: ty,
      case_indet: scrut.expr_indet || rules_indet,
    };
  };
}

and transform_rule =
    (
      ctx: context,
      rule: DHExp.rule,
      scrut_ty: HTyp.t,
      scrut_indet: Hir.has_indet,
    )
    : Hir.rule => {
  switch (rule) {
  | Rule(dp, d) =>
    let (dp, ctx') = transform_pat(ctx, dp, scrut_ty, scrut_indet);
    let d = transform_exp(ctx', d);
    {
      rule_kind: ERule(dp, d),
      rule_ty: d.expr_ty,
      rule_indet: dp.pat_indet || d.expr_indet,
    };
  };
}

and transform_var_map =
    (ctx: context, sigma: VarMap.t_(DHExp.t))
    : (VarMap.t_(Hir.expr), Hir.has_indet) => {
  let sigma = sigma |> List.map(((x, d)) => (x, transform_exp(ctx, d)));
  let sigma_indet =
    sigma |> List.exists(((_, d): (Var.t, Hir.expr)) => d.expr_indet);
  (sigma, sigma_indet);
}

and transform_pat =
    (ctx: context, dp: DHPat.t, ty: HTyp.t, expr_indet: Hir.has_indet)
    : (Hir.pat, context) => {
  switch (dp) {
  | EmptyHole(u, i) => ({pat_kind: PEmptyHole(u, i), pat_indet: true}, ctx)

  | NonEmptyHole(reason, u, i, dp) =>
    let (dp, ctx) = transform_pat(ctx, dp, ty, expr_indet);
    ({pat_kind: PNonEmptyHole(reason, u, i, dp), pat_indet: true}, ctx);

  | Keyword(u, i, k) => (
      {pat_kind: PKeyword(u, i, k), pat_indet: true},
      ctx,
    )

  | InvalidText(u, i, t) => (
      {pat_kind: PInvalidText(u, i, t), pat_indet: true},
      ctx,
    )

  | Wild => ({pat_kind: PWild, pat_indet: false}, ctx)

  | Var(x) =>
    let ctx' = VarMap.extend(ctx, (x, (ty, expr_indet)));
    ({pat_kind: PVar(x), pat_indet: expr_indet}, ctx');

  | IntLit(i) => ({pat_kind: PIntLit(i), pat_indet: false}, ctx)

  | FloatLit(f) => ({pat_kind: PFloatLit(f), pat_indet: false}, ctx)

  | BoolLit(b) => ({pat_kind: PBoolLit(b), pat_indet: false}, ctx)

  | Inj(side, dp') =>
    switch (side, ty) {
    | (L, Sum(ty, _))
    | (R, Sum(_, ty)) =>
      let (dp', ctx) = transform_pat(ctx, dp', ty, expr_indet);
      ({pat_kind: PInj(side, dp'), pat_indet: dp'.pat_indet}, ctx);
    | _ => raise(WrongTypeError)
    }

  | ListNil => ({pat_kind: PListNil, pat_indet: false}, ctx)

  | Cons(dp, dps) =>
    switch (ty) {
    | List(ty') =>
      let (dp, ctx) = transform_pat(ctx, dp, ty', expr_indet);
      let (dps, ctx) = transform_pat(ctx, dps, ty, expr_indet);
      (
        {pat_kind: PCons(dp, dps), pat_indet: dp.pat_indet || dps.pat_indet},
        ctx,
      );
    | _ => raise(WrongTypeError)
    }

  | Pair(dp1, dp2) =>
    switch (ty) {
    | Prod([dp1_ty, dp2_ty]) =>
      let (dp1, ctx) = transform_pat(ctx, dp1, dp1_ty, expr_indet);
      let (dp2, ctx) = transform_pat(ctx, dp2, dp2_ty, expr_indet);
      (
        {
          pat_kind: PPair(dp1, dp2),
          pat_indet: dp1.pat_indet || dp2.pat_indet,
        },
        ctx,
      );
    | _ => raise(WrongTypeError)
    }

  | Triv => ({pat_kind: PTriv, pat_indet: false}, ctx)

  | Ap(dp1, dp2) =>
    switch (ty) {
    | Arrow(dp1_ty, dp2_ty) =>
      let (dp1, ctx) = transform_pat(ctx, dp1, dp1_ty, expr_indet);
      let (dp2, ctx) = transform_pat(ctx, dp2, dp2_ty, expr_indet);
      (
        {pat_kind: PAp(dp1, dp2), pat_indet: dp1.pat_indet || dp2.pat_indet},
        ctx,
      );
    | _ => raise(WrongTypeError)
    }
  };
};

let transform = (ctx: context, d: DHExp.t): Hir.expr =>
  transform_exp(ctx, d);
