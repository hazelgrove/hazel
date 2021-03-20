open Sexplib.Std;

[@deriving sexp]
type step_result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t)
  | Step(DHExp.t);

exception InvalidInput(int);

let rec step = (d: DHExp.t): step_result =>
  switch (d) {
  | BoundVar(_) => raise(InvalidInput(1))
  | Let(dp, d1, d2) =>
    switch (step(d1)) {
    | Step(d1') => Step(Let(dp, d1', d2))
    | BoxedValue(d1)
    | Indet(d1) =>
      switch (Elaborator_Exp.matches(dp, d1)) {
      | Indet => Indet(d)
      | DoesNotMatch => Indet(d)
      | Matches(env) => Step(Elaborator_Exp.subst(env, d2))
      }
    }
  | FixF(x, _, d1) => Step(Elaborator_Exp.subst_var(d, x, d1))
  | Lam(_, _, _) => BoxedValue(d)
  | Ap(d1, d2) =>
    switch (step(d1)) {
    | Step(d1') => Step(Ap(d1', d2))
    | BoxedValue(Lam(dp, _, d3)) =>
      switch (step(d2)) {
      | Step(d2') => Step(Ap(d1, d2'))
      | BoxedValue(d2)
      | Indet(d2) =>
        switch (Elaborator_Exp.matches(dp, d2)) {
        | DoesNotMatch => Indet(d)
        | Indet => Indet(d)
        | Matches(env) =>
          /* beta rule */
          Step(Elaborator_Exp.subst(env, d3))
        }
      }
    | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      switch (step(d2)) {
      | Step(d2') => Step(Ap(d1, d2'))
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        Step(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
      }
    | BoxedValue(_) => raise(InvalidInput(2))
    | Indet(d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(Ap(d1, d2'))
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2'))
      }
    }
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => BoxedValue(d)
  | BinBoolOp(op, d1, d2) =>
    switch (step(d1)) {
    | Step(d1') => Step(BinBoolOp(op, d1', d2))
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(BinBoolOp(op, d1, d2'))
      | BoxedValue(BoolLit(b2)) =>
        BoxedValue(EvaluatorCommon.eval_bin_bool_op(op, b1, b2))
      | BoxedValue(_) => raise(InvalidInput(3))
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
      }
    | BoxedValue(_) => raise(InvalidInput(4))
    | Indet(d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(BinBoolOp(op, d1, d2'))
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (step(d1)) {
    | Step(d1') => Step(BinIntOp(op, d1', d2))
    | BoxedValue(IntLit(n1) as d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(BinIntOp(op, d1, d2'))
      | BoxedValue(IntLit(n2)) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Step(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              DivideByZero,
            ),
          )
        | _ => Step(EvaluatorCommon.eval_bin_int_op(op, n1, n2))
        }
      | BoxedValue(_) => raise(InvalidInput(3))
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    | BoxedValue(_) => raise(InvalidInput(4))
    | Indet(d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(BinIntOp(op, d1, d2'))
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (step(d1)) {
    | Step(d1') => Step(BinFloatOp(op, d1', d2))
    | BoxedValue(FloatLit(f1) as d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2'))
      | BoxedValue(FloatLit(f2)) =>
        Step(EvaluatorCommon.eval_bin_float_op(op, f1, f2))
      | BoxedValue(_) => raise(InvalidInput(8))
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    | BoxedValue(_) => raise(InvalidInput(7))
    | Indet(d1') =>
      switch (step(d2)) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2'))
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    }
  | Inj(ty, side, d1) =>
    switch (step(d1)) {
    | Step(d1') => Step(Inj(ty, side, d1'))
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1'))
    | Indet(d1') => Indet(Inj(ty, side, d1'))
    }
  | Pair(d1, d2) =>
    switch (step(d1), step(d2)) {
    | (Step(d1'), _) => Step(Pair(d1', d2))
    | (_, Step(d2')) => Step(Pair(d1, d2'))
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2))
    | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Pair(d1, d2))
    }
  | Cons(d1, d2) =>
    switch (step(d1), step(d2)) {
    | (Step(d1'), _) => Step(Cons(d1', d2))
    | (_, Step(d2')) => Step(Cons(d1, d2'))
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2))
    | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Cons(d1, d2))
    }
  | ConsistentCase(Case(d1, rules, n)) => evaluate_case(None, d1, rules, n)
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
    evaluate_case(Some((u, i, sigma)), d1, rules, n)
  | EmptyHole(_) => Indet(d)
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    switch (step(d1)) {
    | Step(d1') => Step(NonEmptyHole(reason, u, i, sigma, d1'))
    | BoxedValue(d1')
    | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
    }
  | FreeVar(_) => Indet(d)
  | Keyword(_) => Indet(d)
  | InvalidText(_) => Indet(d)
  | Cast(d1, ty, ty') =>
    switch (step(d1)) {
    | Step(d1') => Step(Cast(d1', ty, ty'))
    | BoxedValue(d1') as result =>
      switch (
        EvaluatorCommon.ground_cases_of(ty),
        EvaluatorCommon.ground_cases_of(ty'),
      ) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(d1', ty, ty'))
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            BoxedValue(d1'');
          } else {
            Indet(FailedCast(d1', ty, ty'));
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          raise(InvalidInput(6))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        Step(DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty'))
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        Step(DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty'))
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(d1', ty, ty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          result;
        } else {
          BoxedValue(Cast(d1', ty, ty'));
        }
      }
    | Indet(d1') as result =>
      switch (
        EvaluatorCommon.ground_cases_of(ty),
        EvaluatorCommon.ground_cases_of(ty'),
      ) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(d1', ty, ty'))
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            Indet(d1'');
          } else {
            Indet(FailedCast(d1', ty, ty'));
          }
        | _ => Indet(Cast(d1', ty, ty'))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        Step(DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty'))
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        Step(DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty'))
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(d1', ty, ty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          result;
        } else {
          Indet(Cast(d1', ty, ty'));
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (step(d1)) {
    | Step(d1') => Step(FailedCast(d1', ty, ty'))
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty'))
    }
  | InvalidOperation(_, _) => Indet(d)
  }
and evaluate_case =
    (
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : step_result =>
  switch (step(scrut)) {
  | Step(scrut') =>
    let case = DHExp.Case(scrut', rules, current_rule_index);
    switch (inconsistent_info) {
    | None => Step(ConsistentCase(case))
    | Some((u, i, sigma)) => Step(InconsistentBranches(u, i, sigma, case))
    };
  | BoxedValue(scrut)
  | Indet(scrut) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      switch (inconsistent_info) {
      | None => Indet(ConsistentCase(case))
      | Some((u, i, sigma)) =>
        Indet(InconsistentBranches(u, i, sigma, case))
      };
    | Some(Rule(dp, d)) =>
      switch (Elaborator_Exp.matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Matches(env) => Step(Elaborator_Exp.subst(env, d))
      | DoesNotMatch =>
        evaluate_case(inconsistent_info, scrut, rules, current_rule_index + 1)
      }
    }
  };

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

let is_final = (d: DHExp.t): bool =>
  switch (step(d)) {
  | Step(_) => false
  | BoxedValue(_)
  | Indet(_) => true
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

let ctx_step = (d: DHExp.t): DHExp.t =>
  if (is_final(d)) {
    d;
  } else {
    let (ctx, d0) = decompose(d);
    switch (step(d0)) {
    | BoxedValue(d0')
    | Indet(d0')
    | Step(d0') => compose((ctx, d0'))
    };
  };

let rec steps = (d: DHExp.t): DHExp.t => {
  let d' = ctx_step(d);
  // print_string(d' |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string);
  d' == d ? d : steps(d');
};

let step_evaluate = (d: DHExp.t): option(DHExp.t) =>
  try(Some(steps(d))) {
  | InvalidInput(_) => None
  };

let rec quick_steps = (d: DHExp.t): step_result =>
  switch (step(d)) {
  | Step(d0) => quick_steps(d0)
  | Indet(d) => Indet(d)
  | BoxedValue(d) => BoxedValue(d)
  };

let quick_step_evaluate = (d: DHExp.t): Evaluator.result =>
  try(
    switch (quick_steps(d)) {
    | Step(d)
    | Indet(d) => Indet(d)
    | BoxedValue(d) => BoxedValue(d)
    }
  ) {
  | InvalidInput(i) => InvalidInput(i)
  };

let step_evaluate_web = (d: DHExp.t): Evaluator.result =>
  //for hazelweb
  try(
    switch (step(steps(d))) {
    | Step(d)
    | Indet(d) => Indet(d)
    | BoxedValue(d) => BoxedValue(d)
    }
  ) {
  | InvalidInput(i) => InvalidInput(i)
  };
