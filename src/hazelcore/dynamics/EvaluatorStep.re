open Sexplib.Std;

module EvalCtx = {
  [@deriving sexp]
  type t =
    | Mark
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Pair1(t, DHExp.t)
    | Pair2(DHExp.t, t)
    | Let(DHPat.t, t, DHExp.t)
    | Inj(HTyp.t, InjSide.t, t)
    | NonEmptyHole(
        ErrStatus.HoleReason.t,
        MetaVar.t,
        MetaVarInst.t,
        VarMap.t_(DHExp.t),
        t,
      )
    | Cast(t, HTyp.t, HTyp.t)
    | FailedCast(t, HTyp.t, HTyp.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | ConsistentCase(t, list(DHExp.rule), int)
    | InconsistentBranches(
        MetaVar.t,
        MetaVarInst.t,
        VarMap.t_(DHExp.t),
        t,
        list(DHExp.rule),
        int,
      );
};

let rec is_boxedval = (d: DHExp.t): bool =>
  switch (d) {
  | BoundVar(_)
  | Lam(_, _, _)
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => true
  | Pair(d1, d2) => is_boxedval(d1) && is_boxedval(d2)
  | Inj(_, _, d1) => is_boxedval(d1)
  | Cons(d1, d2) => is_boxedval(d1) && is_boxedval(d2)
  | Cast(d1, Arrow(ty1, ty2), Arrow(ty3, ty4)) =>
    is_boxedval(d1) && !(HTyp.eq(ty1, ty3) && HTyp.eq(ty2, ty4))
  | Cast(d1, Sum(ty1, ty2), Sum(ty3, ty4)) =>
    is_boxedval(d1) && !(HTyp.eq(ty1, ty3) && HTyp.eq(ty2, ty4))
  | Cast(d1, List(ty1), List(ty2)) => is_boxedval(d1) && !HTyp.eq(ty1, ty2)
  | Cast(d1, Prod(tys1), Prod(tys2)) =>
    is_boxedval(d1)
    && !(List.map2(HTyp.eq, tys1, tys2) |> List.fold_left((&&), true))
  | Cast(d1, ty, Hole) =>
    EvaluatorCommon.ground_cases_of(ty) == Ground && is_boxedval(d1)
  | _ => false
  };

let rec is_final = (d: DHExp.t): bool => is_boxedval(d) || is_indet(d)
and is_indet = (d: DHExp.t): bool =>
  switch (d) {
  | Ap(Cast(_, Arrow(_, _), Arrow(_, _)), _) => false
  | FreeVar(_)
  | Keyword(_)
  | InvalidText(_)
  | EmptyHole(_, _, _) => true
  | NonEmptyHole(_, _, _, _, d1) => is_final(d1)
  | InvalidOperation(d1, _) => is_final(d1)
  | Cast(d1, ty, Hole) =>
    is_indet(d1) && EvaluatorCommon.ground_cases_of(ty) == Ground
  | Cast(Cast(_, _, Hole), Hole, _) => false
  | Cast(d1, Hole, ty) =>
    is_indet(d1) && EvaluatorCommon.ground_cases_of(ty) == Ground
  | Cast(d, Arrow(ty1, ty2), Arrow(ty3, ty4)) =>
    is_indet(d) && !(HTyp.eq(ty1, ty3) && HTyp.eq(ty2, ty4))
  | FailedCast(d1, ty1, ty2) =>
    is_final(d1)
    && EvaluatorCommon.ground_cases_of(ty1) == Ground
    && EvaluatorCommon.ground_cases_of(ty2) == Ground
    && !HTyp.eq(ty1, ty2) // why we need this?
  | Ap(d1, d2) => is_indet(d1) && is_final(d2)
  | Inj(_, _, d1) => is_indet(d1)
  | Pair(d1, d2) => is_indet(d1) && is_final(d2) || is_indet(d2)
  | Cons(d1, d2) => is_indet(d1) && is_final(d2) || is_indet(d2)
  | BinIntOp(_, d1, d2) => is_indet(d1) && is_final(d2) || is_indet(d2)
  | BinBoolOp(_, d1, d2) => is_indet(d1) && is_final(d2) || is_indet(d2)
  | BinFloatOp(_, d1, d2) => is_indet(d1) && is_final(d2) || is_indet(d2)
  | _ => false
  };

let rec decompose = (d: DHExp.t): (EvalCtx.t, DHExp.t) =>
  switch (d) {
  | EmptyHole(_, _, _)
  | Keyword(_, _, _, _)
  | FreeVar(_, _, _, _)
  | InvalidText(_, _, _, _)
  | BoundVar(_)
  | Lam(_, _, _)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv
  | FixF(_, _, _) => (Mark, d)
  | Ap(d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Ap2(d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Ap1(ctx, d2), d0);
    }
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (NonEmptyHole(reason, u, i, sigma, ctx), d0);
    }
  | BinBoolOp(op, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (BinBoolOp2(op, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (BinBoolOp1(op, ctx, d2), d0);
    }
  | BinIntOp(op, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (BinIntOp2(op, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (BinIntOp1(op, ctx, d2), d0);
    }
  | BinFloatOp(op, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (BinFloatOp2(op, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (BinFloatOp1(op, ctx, d2), d0);
    }
  | Cons(d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Cons2(d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Cons1(ctx, d2), d0);
    }
  | Cast(d1, ty1, ty2) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (Cast(ctx, ty1, ty2), d0);
    }
  | FailedCast(d1, ty1, ty2) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (FailedCast(ctx, ty1, ty2), d0);
    }
  | Pair(d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Pair2(d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Pair1(ctx, d2), d0);
    }
  | Let(dp, d1, d2) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (Let(dp, ctx, d2), d0);
    }
  | Inj(ty, side, d1) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (Inj(ty, side, ctx), d0);
    }
  | InvalidOperation(d1, err) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (InvalidOperation(ctx, err), d0);
    }
  | ConsistentCase(Case(d1, rule, n)) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (ConsistentCase(ctx, rule, n), d0);
    }
  | InconsistentBranches(u, i, sigma, Case(d1, rule, n)) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (InconsistentBranches(u, i, sigma, ctx, rule, n), d0);
    }
  };

let rec compose = ((ctx, d): (EvalCtx.t, DHExp.t)): DHExp.t =>
  switch (ctx) {
  | Mark => d
  | Ap1(ctx1, d1) => Ap(compose((ctx1, d)), d1)
  | Ap2(d1, ctx1) => Ap(d1, compose((ctx1, d)))
  | BinBoolOp1(op, ctx1, d1) => BinBoolOp(op, compose((ctx1, d)), d1)
  | BinBoolOp2(op, d1, ctx1) => BinBoolOp(op, d1, compose((ctx1, d)))
  | BinIntOp1(op, ctx1, d1) => BinIntOp(op, compose((ctx1, d)), d1)
  | BinIntOp2(op, d1, ctx1) => BinIntOp(op, d1, compose((ctx1, d)))
  | BinFloatOp1(op, ctx1, d1) => BinFloatOp(op, compose((ctx1, d)), d1)
  | BinFloatOp2(op, d1, ctx1) => BinFloatOp(op, d1, compose((ctx1, d)))
  | Cons1(ctx1, d1) => Cons(compose((ctx1, d)), d1)
  | Cons2(d1, ctx1) => Cons(d1, compose((ctx1, d)))
  | Pair1(ctx1, d1) => Pair(compose((ctx1, d)), d1)
  | Pair2(d1, ctx1) => Pair(d1, compose((ctx1, d)))
  | Let(dp, ctx1, d1) => Let(dp, compose((ctx1, d)), d1)
  | Inj(ty, side, ctx1) => Inj(ty, side, compose((ctx1, d)))
  | Cast(ctx1, ty1, ty2) => Cast(compose((ctx1, d)), ty1, ty2)
  | FailedCast(ctx1, ty1, ty2) => FailedCast(compose((ctx1, d)), ty1, ty2)
  | InvalidOperation(ctx1, err) => InvalidOperation(compose((ctx1, d)), err)
  | NonEmptyHole(reason, u, i, sigma, ctx1) =>
    NonEmptyHole(reason, u, i, sigma, compose((ctx1, d)))
  | ConsistentCase(ctx1, rule, n) =>
    ConsistentCase(Case(compose((ctx1, d)), rule, n))
  | InconsistentBranches(u, i, sigma, ctx1, rule, n) =>
    InconsistentBranches(u, i, sigma, Case(compose((ctx1, d)), rule, n))
  };

[@deriving sexp]
type step_result =
  | Final
  | Step(DHExp.t)
  | InvalidInput(int);

let evaluate_case_instruction =
    (
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : step_result =>
  switch (List.nth_opt(rules, current_rule_index)) {
  | None => Final
  | Some(Rule(dp, d)) =>
    switch (Elaborator_Exp.matches(dp, scrut)) {
    | Indet => Final
    | Matches(env) => Step(Elaborator_Exp.subst(env, d))
    | DoesNotMatch =>
      let case = DHExp.Case(scrut, rules, current_rule_index + 1);
      switch (inconsistent_info) {
      | None => Step(ConsistentCase(case))
      | Some((u, i, sigma)) => Step(InconsistentBranches(u, i, sigma, case))
      };
    }
  };

let instruction_step = (d: DHExp.t): step_result =>
  switch (d) {
  | Ap(d1, d2) =>
    switch (d1) {
    | Lam(pat, _, d0) =>
      switch (Elaborator_Exp.matches(pat, d2)) {
      | DoesNotMatch => Final
      | Indet => Final // These two Final just mean the result is indet
      | Matches(env) => Step(Elaborator_Exp.subst(env, d0))
      }
    | Cast(d0, Arrow(t1, t2), Arrow(t1', t2')) =>
      if (HTyp.eq(t1, t1') && HTyp.eq(t2, t2')) {
        Final;
      } else {
        Step(Cast(Ap(d0, Cast(d2, t1', t1)), t2, t2'));
      }
    | _ =>
      if (is_boxedval(d1)) {
        InvalidInput(2);
      } else {
        Final;
      }
    }
  | Let(dp, d1, d2) =>
    switch (Elaborator_Exp.matches(dp, d1)) {
    | Indet => Final
    | DoesNotMatch => Final // These two Final just mean the result is indet
    | Matches(env) => Step(Elaborator_Exp.subst(env, d2))
    }
  | BinBoolOp(op, d1, d2) =>
    switch (d1, d2) {
    | (DHExp.BoolLit(b1), DHExp.BoolLit(b2)) =>
      Step(EvaluatorCommon.eval_bin_bool_op(op, b1, b2))
    | (DHExp.BoolLit(_), _) => InvalidInput(3)
    | _ => InvalidInput(4)
    }
  | BinIntOp(op, d1, d2) =>
    switch (op, d1, d2) {
    | (Divide, DHExp.IntLit(i1), DHExp.IntLit(0)) =>
      Step(
        InvalidOperation(
          BinIntOp(op, IntLit(i1), IntLit(0)),
          DivideByZero,
        ),
      )
    | (_, DHExp.IntLit(i1), DHExp.IntLit(i2)) =>
      Step(EvaluatorCommon.eval_bin_int_op(op, i1, i2))
    | (_, DHExp.IntLit(_), _) => InvalidInput(3)
    | _ => InvalidInput(4)
    }
  | BinFloatOp(op, d1, d2) =>
    switch (d1, d2) {
    | (DHExp.FloatLit(f1), DHExp.FloatLit(f2)) =>
      Step(EvaluatorCommon.eval_bin_float_op(op, f1, f2))
    | (DHExp.FloatLit(_), _) => InvalidInput(8)
    | _ => InvalidInput(7)
    }
  | FixF(x, _, d1) => Step(Elaborator_Exp.subst_var(d, x, d1))
  | Cast(d1, ty1, ty2) =>
    switch (
      EvaluatorCommon.ground_cases_of(ty1),
      EvaluatorCommon.ground_cases_of(ty2),
    ) {
    | (Hole, Hole)
    | (Ground, Ground) => Step(d1)
    | (Hole, Ground) =>
      switch (d1) {
      | Cast(d1', ty1', Hole) =>
        if (HTyp.eq(ty1', ty1)) {
          Step(d1');
        } else {
          Step(FailedCast(d1, ty1, ty2));
        }
      | _ => InvalidInput(6)
      }
    | (Hole, NotGroundOrHole(ty2_grounded)) =>
      Step(Cast(Cast(d1, ty1, ty2_grounded), ty2_grounded, ty1))
    | (NotGroundOrHole(ty1_grounded), Hole) =>
      Step(Cast(Cast(d1, ty1, ty1_grounded), ty1_grounded, ty2))
    | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
      if (HTyp.eq(ty1, ty2)) {
        Step(d1);
      } else {
        Final;
      }
    | _ => Final
    }
  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case_instruction(None, d1, rules, n);
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
    evaluate_case_instruction(Some((u, i, sigma)), d1, rules, n);
  | BoundVar(_) => InvalidInput(1)
  | EmptyHole(_, _, _)
  | NonEmptyHole(_, _, _, _, _)
  | Keyword(_, _, _, _)
  | FreeVar(_, _, _, _)
  | InvalidText(_, _, _, _)
  | Inj(_, _, _)
  | FailedCast(_, _, _)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Cons(_, _)
  | Pair(_, _)
  | Triv
  | Lam(_, _, _)
  | InvalidOperation(_, _) => Final
  //| _ => None
  };

let evaluate_step = (d: DHExp.t): step_result => {
  let (ctx, d0) = decompose(d);
  switch (instruction_step(d0)) {
  | InvalidInput(i) => InvalidInput(i)
  | Final => Final
  | Step(d1) => Step(compose((ctx, d1)))
  };
};

let rec evaluate_steps = (d: DHExp.t): option(DHExp.t) => {
  switch (evaluate_step(d)) {
  | Step(d0) => evaluate_steps(d0)
  | Final => Some(d)
  | InvalidInput(i) => None
  };
};
