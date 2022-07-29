open TransformMonad;
open TransformMonad.Syntax;
open Expr;
open HTyp;

type m('a) = TransformMonad.t('a);

exception FixFError;
exception FreeVarError;
exception WrongTypeError;

let rec transform_exp = (ctx: Contexts.t, d: DHExp.t): m((Expr.t, HTyp.t)) => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let* sigma = transform_var_map(ctx, sigma);
    let+ label = next_expr_label;
    ({kind: EEmptyHole(u, i, sigma), label}, Hole);

  | NonEmptyHole(reason, u, i, sigma, d') =>
    let* sigma = transform_var_map(ctx, sigma);
    let* (d', _) = transform_exp(ctx, d');
    let+ label = next_expr_label;
    ({kind: ENonEmptyHole(reason, u, i, sigma, d'), label}, Hole);

  | ExpandingKeyword(u, i, sigma, k) =>
    let* sigma = transform_var_map(ctx, sigma);
    let+ label = next_expr_label;
    ({kind: EKeyword(u, i, sigma, k), label}, Hole);

  | FreeVar(u, i, sigma, k) =>
    let* sigma = transform_var_map(ctx, sigma);
    let+ label = next_expr_label;
    ({kind: EFreeVar(u, i, sigma, k), label}, Hole);

  | InvalidText(u, i, sigma, text) =>
    let* sigma = transform_var_map(ctx, sigma);
    let+ label = next_expr_label;
    ({kind: EInvalidText(u, i, sigma, text), label}, Hole);

  | BoundVar(x) =>
    switch (VarMap.lookup(Contexts.gamma(ctx), x)) {
    | Some(ty) =>
      let+ label = next_expr_label;
      ({kind: EBoundVar(ty, x), label}, ty);
    | None => raise(FreeVarError)
    }

  | FixF(_) => raise(FixFError)
  | Let(Var(_), FixF(x, ty, Fun(dp, dp_ty, d3)), body) =>
    // TODO: Not really sure if any of this recursive function handling is right...
    let* (dp, ctx) = transform_pat(ctx, dp, ty);
    let ctx = VarMap.extend(ctx, (x, ty));

    let* (d3, _) = transform_exp(ctx, d3);
    let* (body, body_ty) = transform_exp(ctx, body);
    let+ label = next_expr_label;
    ({kind: ELetRec(x, dp, dp_ty, d3, body), label}, body_ty);

  | Let(dp, d', body) =>
    let* (d', d'_ty) = transform_exp(ctx, d');
    let* (dp, body_ctx) = transform_pat(ctx, dp, d'_ty);
    let* (body, body_ty) = transform_exp(body_ctx, body);
    let+ label = next_expr_label;
    ({kind: ELet(dp, d', body), label}, body_ty);

  | Fun(dp, dp_ty, body) =>
    let* (dp, body_ctx) = transform_pat(ctx, dp, dp_ty);
    let* (body, body_ty) = transform_exp(body_ctx, body);
    let+ label = next_expr_label;
    ({kind: EFun(dp, dp_ty, body), label}, Arrow(dp_ty, body_ty));

  | Ap(fn, arg) =>
    let* (fn, fn_ty) = transform_exp(ctx, fn);
    let* (arg, _) = transform_exp(ctx, arg);
    switch (fn.kind) {
    // TODO: expand arrow casts and do transform_exp recursively here
    | ECast(_fn, _ty1, _ty2) => failwith("FnCastExpansion")
    | EFun(_, _, _) =>
      switch (fn_ty) {
      | Arrow(_, ty') =>
        let+ label = next_expr_label;
        ({kind: EAp(fn, arg), label}, ty');
      | _ => raise(WrongTypeError)
      }
    | _ => failwith("NotImplemented")
    };

  | ApBuiltin(name, args) =>
    let* args =
      args
      |> List.map(arg => {
           let+ (arg, _) = transform_exp(ctx, arg);
           arg;
         })
      |> sequence;

    switch (VarMap.lookup(Contexts.gamma(ctx), name)) {
    | Some(Arrow(_, ty')) =>
      let+ label = next_expr_label;
      ({kind: EApBuiltin(name, args), label}, ty');
    | _ => raise(WrongTypeError)
    };

  | BinBoolOp(op, d1, d2) =>
    let op = transform_bool_op(op);
    let* (d1, _) = transform_exp(ctx, d1);
    let* (d2, _) = transform_exp(ctx, d2);

    let+ label = next_expr_label;
    ({kind: EBinBoolOp(op, d1, d2), label}, Bool);

  | BinIntOp(op, d1, d2) =>
    let op = transform_int_op(op);
    let* (d1, _) = transform_exp(ctx, d1);
    let* (d2, _) = transform_exp(ctx, d2);

    let+ label = next_expr_label;
    ({kind: EBinIntOp(op, d1, d2), label}, Int);

  | BinFloatOp(op, d1, d2) =>
    let op = transform_float_op(op);
    let* (d1, _) = transform_exp(ctx, d1);
    let* (d2, _) = transform_exp(ctx, d2);

    let+ label = next_expr_label;
    ({kind: EBinFloatOp(op, d1, d2), label}, Float);

  | Pair(d1, d2) =>
    let* (d1, d1_ty) = transform_exp(ctx, d1);
    let* (d2, d2_ty) = transform_exp(ctx, d2);
    let+ label = next_expr_label;
    ({kind: EPair(d1, d2), label}, Prod([d1_ty, d2_ty]));

  | Cons(d1, d2) =>
    let* (d1, _) = transform_exp(ctx, d1);
    let* (d2, d2_ty) = transform_exp(ctx, d2);
    let+ label = next_expr_label;
    ({kind: ECons(d1, d2), label}, d2_ty);

  | Inj(other_ty, side, d') =>
    let* (d', d'_ty) = transform_exp(ctx, d');
    let ty: HTyp.t =
      switch (side) {
      | L => Sum(d'_ty, other_ty)
      | R => Sum(other_ty, d'_ty)
      };

    let+ label = next_expr_label;
    ({kind: EInj(ty, side, d'), label}, ty);

  | BoolLit(b) =>
    let+ label = next_expr_label;
    ({kind: EBoolLit(b), label}, Bool);

  | IntLit(i) =>
    let+ label = next_expr_label;
    ({kind: EIntLit(i), label}, Int);

  | FloatLit(f) =>
    let+ label = next_expr_label;
    ({kind: EFloatLit(f), label}, Float);

  | ListNil(ty) =>
    let+ label = next_expr_label;
    ({kind: ENil(ty), label}, List(ty));

  | Triv =>
    let+ label = next_expr_label;
    ({kind: ETriv, label}, Prod([]));

  | ConsistentCase(case) =>
    let* (case, case_ty) = transform_case(ctx, case);
    let+ label = next_expr_label;
    ({kind: EConsistentCase(case), label}, case_ty);

  | InconsistentBranches(u, i, sigma, case) =>
    let* sigma = transform_var_map(ctx, sigma);
    let* (case, _) = transform_case(ctx, case);
    let+ label = next_expr_label;
    ({kind: EInconsistentBranches(u, i, sigma, case), label}, Hole);

  | Cast(d', ty, ty') =>
    // FIXME: default implementation of Cast
    // let (d', _) = transform_exp(ctx, d');
    // ({kind: ECast(d', ty, ty')}, ty');
    switch (HTyp.ground_cases_of(ty), HTyp.ground_cases_of(ty')) {
    | (GNotGroundOrHole(_), GNotGroundOrHole(_)) =>
      if (HTyp.eq(ty, ty')) {
        transform_exp(ctx, d');
      } else {
        let* (d', _) = transform_exp(ctx, d');
        let+ label = next_expr_label;
        ({kind: ECast(d', ty, ty'), label}, ty');
      }
    | _ =>
      let* (d', _) = transform_exp(ctx, d);
      let+ label = next_expr_label;
      ({kind: ECast(d', ty, ty'), label}, ty');
    }

  | FailedCast(d', ty, ty') =>
    let* (d', _) = transform_exp(ctx, d');
    let+ label = next_expr_label;
    ({kind: EFailedCast(d', ty, ty'), label}, ty');

  | InvalidOperation(d', err) =>
    let* (d', d'_ty) = transform_exp(ctx, d');
    let+ label = next_expr_label;
    ({kind: EInvalidOperation(d', err), label}, d'_ty);
  };
}

and transform_bool_op = (op: DHExp.BinBoolOp.t): Expr.bin_bool_op =>
  switch (op) {
  | And => Expr.OpAnd
  | Or => Expr.OpOr
  }

and transform_int_op = (op: DHExp.BinIntOp.t): Expr.bin_int_op => {
  switch (op) {
  | Minus => Expr.OpMinus
  | Plus => Expr.OpPlus
  | Times => Expr.OpTimes
  | Divide => Expr.OpDivide
  | LessThan => Expr.OpLessThan
  | GreaterThan => Expr.OpGreaterThan
  | Equals => Expr.OpEquals
  };
}

and transform_float_op = (op: DHExp.BinFloatOp.t): Expr.bin_float_op => {
  switch (op) {
  | FMinus => Expr.OpFMinus
  | FPlus => Expr.OpFPlus
  | FTimes => Expr.OpFTimes
  | FDivide => Expr.OpFDivide
  | FLessThan => Expr.OpFLessThan
  | FGreaterThan => Expr.OpFGreaterThan
  | FEquals => Expr.OpFEquals
  };
}

and transform_case =
    (ctx: Contexts.t, case: DHExp.case): m((Expr.case, HTyp.t)) => {
  switch (case) {
  // TODO: Check that all rules have same type?
  | Case(scrut, rules, i) =>
    let* (scrut, scrut_ty) = transform_exp(ctx, scrut);
    let+ rules =
      rules
      |> List.map(rule => transform_rule(ctx, rule, scrut_ty))
      |> sequence;

    let rules_ty = rules |> List.hd |> snd;
    let rules = rules |> List.map(fst);

    ({case_kind: ECase(scrut, rules, i)}, rules_ty);
  };
}

and transform_rule =
    (ctx: Contexts.t, rule: DHExp.rule, scrut_ty: HTyp.t)
    : m((Expr.rule, HTyp.t)) => {
  switch (rule) {
  | Rule(dp, d) =>
    let* (dp, ctx') = transform_pat(ctx, dp, scrut_ty);
    let* (d, d_ty) = transform_exp(ctx', d);
    let+ rule_label = next_rule_label;
    ({rule_kind: ERule(dp, d), rule_label}, d_ty);
  };
}

and transform_var_map =
    (ctx: Contexts.t, sigma: VarMap.t_(DHExp.t)): m(VarMap.t_(Expr.t)) =>
  sigma
  |> List.map(((x, d)) => {
       let+ (d, _) = transform_exp(ctx, d);
       (x, d);
     })
  |> sequence

and transform_pat =
    (ctx: Contexts.t, dp: DHPat.t, ty: HTyp.t): m((Pat.t, Contexts.t)) => {
  Pat.(
    switch (dp) {
    | EmptyHole(u, i) =>
      let+ label = next_pat_label;
      (Pat.{kind: PEmptyHole(u, i), label}, ctx);

    | NonEmptyHole(reason, u, i, dp) =>
      let* (dp, ctx) = transform_pat(ctx, dp, ty);
      let+ label = next_pat_label;
      ({kind: PNonEmptyHole(reason, u, i, dp), label}, ctx);

    | ExpandingKeyword(u, i, k) =>
      let+ label = next_pat_label;
      ({kind: PKeyword(u, i, k), label}, ctx);

    | InvalidText(u, i, t) =>
      let+ label = next_pat_label;
      ({kind: PInvalidText(u, i, t), label}, ctx);

    | Wild =>
      let+ label = next_pat_label;
      ({kind: PWild, label}, ctx);

    | Ap(dp1, dp2) =>
      /* FIXME: Hole type scrutinee? */
      switch (ty) {
      | Arrow(dp1_ty, dp2_ty) =>
        let* (dp1, ctx) = transform_pat(ctx, dp1, dp1_ty);
        let* (dp2, ctx) = transform_pat(ctx, dp2, dp2_ty);
        let+ label = next_pat_label;
        ({kind: PAp(dp1, dp2), label}, ctx);
      | _ => raise(WrongTypeError)
      }

    | Pair(dp1, dp2) =>
      switch (ty) {
      | Prod([dp1_ty, dp2_ty]) =>
        let* (dp1, ctx) = transform_pat(ctx, dp1, dp1_ty);
        let* (dp2, ctx) = transform_pat(ctx, dp2, dp2_ty);
        let+ label = next_pat_label;
        ({kind: PPair(dp1, dp2), label}, ctx);
      | _ => raise(WrongTypeError)
      }

    | Cons(dp, dps) =>
      switch (ty) {
      | List(ty') =>
        let* (dp, ctx) = transform_pat(ctx, dp, ty');
        let* (dps, ctx) = transform_pat(ctx, dps, ty);
        let+ label = next_pat_label;
        ({kind: PCons(dp, dps), label}, ctx);
      | _ => raise(WrongTypeError)
      }

    | Var(x) =>
      let gamma' = VarMap.extend(Contexts.gamma(ctx), (x, ty));
      let+ label = next_pat_label;
      ({kind: PVar(x), label}, gamma');

    | IntLit(i) =>
      let+ label = next_pat_label;
      ({kind: PIntLit(i), label}, ctx);

    | FloatLit(f) =>
      let+ label = next_pat_label;
      ({kind: PFloatLit(f), label}, ctx);

    | BoolLit(b) =>
      let+ label = next_pat_label;
      ({kind: PBoolLit(b), label}, ctx);

    | Inj(side, dp') =>
      switch (side, ty) {
      | (L, Sum(ty, _))
      | (R, Sum(_, ty)) =>
        let* (dp', ctx) = transform_pat(ctx, dp', ty);
        let+ label = next_pat_label;
        ({kind: PInj(side, dp'), label}, ctx);
      | _ => raise(WrongTypeError)
      }

    | ListNil =>
      let+ label = next_pat_label;
      ({kind: PNil, label}, ctx);

    | Triv =>
      let+ label = next_pat_label;
      ({kind: PTriv, label}, ctx);
    }
  );
};

let transform = (ctx: Contexts.t, d: DHExp.t) => {
  let (_, (e, _)) = transform_exp(ctx, d, TransformMonad.init);
  e;
};
