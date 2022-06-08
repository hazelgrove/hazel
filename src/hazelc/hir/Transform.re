exception FixFError;
exception FreeVarError;
exception WrongTypeError;

let rec transform_exp = (ctx: Contexts.t, d: DHExp.t): (Hir.expr, HTyp.t) => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let sigma = transform_var_map(ctx, sigma);
    ({expr_kind: EEmptyHole(u, i, sigma)}, Hole);

  | NonEmptyHole(reason, u, i, sigma, d') =>
    let sigma = transform_var_map(ctx, sigma);
    let (d', _) = transform_exp(ctx, d');
    ({expr_kind: ENonEmptyHole(reason, u, i, sigma, d')}, Hole);

  | Keyword(u, i, sigma, k) =>
    let sigma = transform_var_map(ctx, sigma);
    ({expr_kind: EKeyword(u, i, sigma, k)}, Hole);

  | FreeVar(u, i, sigma, k) =>
    let sigma = transform_var_map(ctx, sigma);
    ({expr_kind: EFreeVar(u, i, sigma, k)}, Hole);

  | InvalidText(u, i, sigma, text) =>
    let sigma = transform_var_map(ctx, sigma);
    ({expr_kind: EInvalidText(u, i, sigma, text)}, Hole);

  | BoundVar(x) =>
    switch (VarMap.lookup(Contexts.gamma(ctx), x)) {
    | Some(ty) => ({expr_kind: EBoundVar(ty, x)}, ty)
    // TODO: Not sure what behavior is needed here.
    | None => raise(FreeVarError)
    }

  | FixF(_) => raise(FixFError)
  | Let(Var(_), FixF(x, ty, Fun(dp, dp_ty, d3)), body) =>
    // TODO: Not really sure if any of this recursive function handling is right...
    let (dp, ctx) = transform_pat(ctx, dp, ty);
    let ctx = VarMap.extend(ctx, (x, ty));

    let (d3, _) = transform_exp(ctx, d3);
    let (body, body_ty) = transform_exp(ctx, body);
    ({expr_kind: ELetRec(x, dp, dp_ty, d3, body)}, body_ty);

  | Let(dp, d', body) =>
    let (d', d'_ty) = transform_exp(ctx, d');
    let (dp, body_ctx) = transform_pat(ctx, dp, d'_ty);
    let (body, body_ty) = transform_exp(body_ctx, body);
    ({expr_kind: ELet(dp, d', body)}, body_ty);

  | Fun(dp, dp_ty, body) =>
    // TODO: Can't assume anything about indet-ness of argument when called?
    let (dp, body_ctx) = transform_pat(ctx, dp, dp_ty);
    let (body, body_ty) = transform_exp(body_ctx, body);
    ({expr_kind: EFun(dp, dp_ty, body)}, Arrow(dp_ty, body_ty));

  | Ap(fn, arg) =>
    let (fn, fn_ty) = transform_exp(ctx, fn);
    let (arg, _) = transform_exp(ctx, arg);
    switch (fn_ty) {
    | Arrow(_, ty') => ({expr_kind: EAp(fn, arg)}, ty')
    | _ => raise(WrongTypeError)
    };

  | ApBuiltin(name, args) =>
    let args =
      args
      |> List.map(arg =>
           switch (transform_exp(ctx, arg)) {
           | (arg, _) => arg
           }
         );

    switch (VarMap.lookup(Contexts.gamma(ctx), name)) {
    | Some(Arrow(_, ty')) => ({expr_kind: EApBuiltin(name, args)}, ty')
    | _ => raise(WrongTypeError)
    };

  | BinBoolOp(op, d1, d2) =>
    let (d1, _) = transform_exp(ctx, d1);
    let (d2, _) = transform_exp(ctx, d2);
    let op = transform_bool_op(op);
    ({expr_kind: EBinBoolOp(op, d1, d2)}, Bool);

  | BinIntOp(op, d1, d2) =>
    let (d1, _) = transform_exp(ctx, d1);
    let (d2, _) = transform_exp(ctx, d2);
    let op = transform_int_op(op);
    ({expr_kind: EBinIntOp(op, d1, d2)}, Int);

  | BinFloatOp(op, d1, d2) =>
    let (d1, _) = transform_exp(ctx, d1);
    let (d2, _) = transform_exp(ctx, d2);
    let op = transform_float_op(op);
    ({expr_kind: EBinFloatOp(op, d1, d2)}, Float);

  | Pair(d1, d2) =>
    let (d1, d1_ty) = transform_exp(ctx, d1);
    let (d2, d2_ty) = transform_exp(ctx, d2);
    ({expr_kind: EPair(d1, d2)}, Prod([d1_ty, d2_ty]));

  | Cons(d1, d2) =>
    let (d1, _) = transform_exp(ctx, d1);
    let (d2, d2_ty) = transform_exp(ctx, d2);
    ({expr_kind: ECons(d1, d2)}, d2_ty);

  | Inj(other_ty, side, d') =>
    let (d', d'_ty) = transform_exp(ctx, d');
    let ty: HTyp.t =
      switch (side) {
      | L => Sum(d'_ty, other_ty)
      | R => Sum(other_ty, d'_ty)
      };
    ({expr_kind: EInj(ty, side, d')}, ty);

  | BoolLit(b) => ({expr_kind: EBoolLit(b)}, Bool)

  | IntLit(i) => ({expr_kind: EIntLit(i)}, Int)

  | FloatLit(f) => ({expr_kind: EFloatLit(f)}, Float)

  | ListNil(ty) => ({expr_kind: ENil(ty)}, List(ty))

  | Triv => ({expr_kind: ETriv}, Prod([]))

  | ConsistentCase(case) =>
    let (case, case_ty) = transform_case(ctx, case);
    ({expr_kind: EConsistentCase(case)}, case_ty);

  | InconsistentBranches(u, i, sigma, case) =>
    let sigma = transform_var_map(ctx, sigma);
    let (case, _) = transform_case(ctx, case);
    ({expr_kind: EInconsistentBranches(u, i, sigma, case)}, Hole);

  | Cast(d', ty, ty') =>
    let (d', _) = transform_exp(ctx, d');
    ({expr_kind: ECast(d', ty, ty')}, ty');

  | FailedCast(d', ty, ty') =>
    let (d', _) = transform_exp(ctx, d');
    ({expr_kind: EFailedCast(d', ty, ty')}, ty');

  | InvalidOperation(d', err) =>
    let (d', d'_ty) = transform_exp(ctx, d');
    ({expr_kind: EInvalidOperation(d', err)}, d'_ty);
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

and transform_case = (ctx: Contexts.t, case: DHExp.case): (Hir.case, HTyp.t) => {
  switch (case) {
  // TODO: Check that all rules have same type.
  | Case(scrut, rules, i) =>
    let (scrut, scrut_ty) = transform_exp(ctx, scrut);
    let (rules_rev, rules_ty) =
      rules
      |> List.fold_left(
           ((rules, _), rule) =>
             switch (transform_rule(ctx, rule, scrut_ty)) {
             | (rule, rule_ty) => ([rule, ...rules], rule_ty)
             },
           ([], HTyp.Hole),
         );
    let rules = List.rev(rules_rev);

    ({case_kind: ECase(scrut, rules, i)}, rules_ty);
  };
}

and transform_rule =
    (ctx: Contexts.t, rule: DHExp.rule, scrut_ty: HTyp.t): (Hir.rule, HTyp.t) => {
  switch (rule) {
  | Rule(dp, d) =>
    let (dp, ctx') = transform_pat(ctx, dp, scrut_ty);
    let (d, d_ty) = transform_exp(ctx', d);
    ({rule_kind: ERule(dp, d)}, d_ty);
  };
}

and transform_var_map =
    (ctx: Contexts.t, sigma: VarMap.t_(DHExp.t)): VarMap.t_(Hir.expr) =>
  sigma
  |> List.map(((x, d)) => {
       let (d, _) = transform_exp(ctx, d);
       (x, d);
     })

and transform_pat =
    (ctx: Contexts.t, dp: DHPat.t, ty: HTyp.t): (Hir.pat, Contexts.t) => {
  switch (dp) {
  | EmptyHole(u, i) => ({pat_kind: PEmptyHole(u, i)}, ctx)

  | NonEmptyHole(reason, u, i, dp) =>
    let (dp, ctx) = transform_pat(ctx, dp, ty);
    ({pat_kind: PNonEmptyHole(reason, u, i, dp)}, ctx);

  | Keyword(u, i, k) => ({pat_kind: PKeyword(u, i, k)}, ctx)

  | InvalidText(u, i, t) => ({pat_kind: PInvalidText(u, i, t)}, ctx)

  | Wild => ({pat_kind: PWild}, ctx)

  | Ap(dp1, dp2) =>
    switch (ty) {
    | Arrow(dp1_ty, dp2_ty) =>
      let (dp1, ctx) = transform_pat(ctx, dp1, dp1_ty);
      let (dp2, ctx) = transform_pat(ctx, dp2, dp2_ty);
      ({pat_kind: PAp(dp1, dp2)}, ctx);
    | _ => raise(WrongTypeError)
    }

  | Pair(dp1, dp2) =>
    switch (ty) {
    | Prod([dp1_ty, dp2_ty]) =>
      let (dp1, ctx) = transform_pat(ctx, dp1, dp1_ty);
      let (dp2, ctx) = transform_pat(ctx, dp2, dp2_ty);
      ({pat_kind: PPair(dp1, dp2)}, ctx);
    | _ => raise(WrongTypeError)
    }

  | Cons(dp, dps) =>
    switch (ty) {
    | List(ty') =>
      let (dp, ctx) = transform_pat(ctx, dp, ty');
      let (dps, ctx) = transform_pat(ctx, dps, ty);
      ({pat_kind: PCons(dp, dps)}, ctx);
    | _ => raise(WrongTypeError)
    }

  | Var(x) =>
    let gamma' = VarMap.extend(Contexts.gamma(ctx), (x, ty));
    ({pat_kind: PVar(x)}, gamma');

  | IntLit(i) => ({pat_kind: PIntLit(i)}, ctx)

  | FloatLit(f) => ({pat_kind: PFloatLit(f)}, ctx)

  | BoolLit(b) => ({pat_kind: PBoolLit(b)}, ctx)

  | Inj(side, dp') =>
    switch (side, ty) {
    | (L, Sum(ty, _))
    | (R, Sum(_, ty)) =>
      let (dp', ctx) = transform_pat(ctx, dp', ty);
      ({pat_kind: PInj(side, dp')}, ctx);
    | _ => raise(WrongTypeError)
    }

  | ListNil => ({pat_kind: PNil}, ctx)

  | Triv => ({pat_kind: PTriv}, ctx)
  };
};

let transform = (ctx: Contexts.t, d: DHExp.t): Hir.expr => {
  let (d, _) = transform_exp(ctx, d);
  d;
};
