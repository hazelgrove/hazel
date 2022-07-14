[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let grounded_Arrow = NotGroundOrHole(Arrow(Hole, Hole));
let grounded_Sum = NotGroundOrHole(Sum(Hole, Hole));
let grounded_Prod = length =>
  NotGroundOrHole(Prod(ListUtil.replicate(length, HTyp.Hole)));
let grounded_List = NotGroundOrHole(List(Hole));

let ground_cases_of = (ty: HTyp.t): ground_cases =>
  switch (ty) {
  | Hole => Hole
  | Bool
  | Int
  | Float
  | Arrow(Hole, Hole)
  | Sum(Hole, Hole)
  | List(Hole) => Ground
  | Prod(tys) =>
    if (List.for_all(HTyp.eq(HTyp.Hole), tys)) {
      Ground;
    } else {
      tys |> List.length |> grounded_Prod;
    }
  | Arrow(_, _) => grounded_Arrow
  | Sum(_, _) => grounded_Sum
  | List(_) => grounded_List
  };

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | Indet;

let rec matches = (dp: DHPat.t, d: DHExp.t): match_result =>
  switch (dp, d) {
  | (_, BoundVar(_)) => DoesNotMatch
  | (EmptyHole(_, _), _)
  | (NonEmptyHole(_, _, _, _), _) => Indet
  | (Wild, _) => Matches(Environment.empty)
  | (Keyword(_, _, _), _) => DoesNotMatch
  | (InvalidText(_), _) => Indet
  | (Var(x), _) =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  | (_, EmptyHole(_, _)) => Indet
  | (_, NonEmptyHole(_, _, _, _)) => Indet
  | (_, FailedCast(_, _, _)) => Indet
  | (_, InvalidOperation(_)) => Indet
  | (_, FreeVar(_, _, _)) => Indet
  | (_, InvalidText(_)) => Indet
  | (_, Let(_, _, _)) => Indet
  | (_, FixF(_, _, _)) => DoesNotMatch
  | (_, Fun(_, _, _)) => DoesNotMatch
  | (_, Ap(_, _)) => Indet
  | (_, BinBoolOp(_, _, _)) => Indet
  | (_, BinIntOp(_, _, _)) => Indet
  | (_, BinFloatOp(_, _, _)) => Indet
  | (_, ConsistentCase(Case(_, _, _))) => Indet

  /* Closure should match like underlying expression */
  | (_, Closure(_, Fun(_))) => DoesNotMatch
  | (_, Closure(_, _)) => Indet

  | (BoolLit(b1), BoolLit(b2)) =>
    if (b1 == b2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (BoolLit(_), Cast(d, Bool, Hole)) => matches(dp, d)
  | (BoolLit(_), Cast(d, Hole, Bool)) => matches(dp, d)
  | (BoolLit(_), _) => DoesNotMatch
  | (IntLit(n1), IntLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (IntLit(_), Cast(d, Int, Hole)) => matches(dp, d)
  | (IntLit(_), Cast(d, Hole, Int)) => matches(dp, d)
  | (IntLit(_), _) => DoesNotMatch
  | (FloatLit(n1), FloatLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (FloatLit(_), Cast(d, Float, Hole)) => matches(dp, d)
  | (FloatLit(_), Cast(d, Hole, Float)) => matches(dp, d)
  | (FloatLit(_), _) => DoesNotMatch
  | (Inj(side1, dp), Inj(_, side2, d)) =>
    switch (side1, side2) {
    | (L, L)
    | (R, R) => matches(dp, d)
    | _ => DoesNotMatch
    }
  | (Inj(side, dp), Cast(d, Sum(tyL1, tyR1), Sum(tyL2, tyR2))) =>
    matches_cast_Inj(side, dp, d, [(tyL1, tyR1, tyL2, tyR2)])
  | (Inj(_, _), Cast(d, Sum(_, _), Hole)) => matches(dp, d)
  | (Inj(_, _), Cast(d, Hole, Sum(_, _))) => matches(dp, d)
  | (Inj(_, _), _) => DoesNotMatch
  | (Pair(dp1, dp2), Pair(d1, d2)) =>
    switch (matches(dp1, d1)) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | (
      Pair(dp1, dp2),
      Cast(d, Prod([head1, ...tail1]), Prod([head2, ...tail2])),
    ) =>
    matches_cast_Pair(
      dp1,
      dp2,
      d,
      [(head1, head2)],
      List.combine(tail1, tail2),
    )
  | (Pair(_, _), Cast(d, Hole, Prod(_)))
  | (Pair(_, _), Cast(d, Prod(_), Hole)) => matches(dp, d)
  | (Pair(_, _), _) => DoesNotMatch
  | (Triv, Triv) => Matches(Environment.empty)
  | (Triv, Cast(d, Hole, Prod([]))) => matches(dp, d)
  | (Triv, Cast(d, Prod([]), Hole)) => matches(dp, d)
  | (Triv, _) => DoesNotMatch
  | (ListNil, ListNil(_)) => Matches(Environment.empty)
  | (ListNil, Cast(d, Hole, List(_))) => matches(dp, d)
  | (ListNil, Cast(d, List(_), Hole)) => matches(dp, d)
  | (ListNil, Cast(d, List(_), List(_))) => matches(dp, d)
  | (ListNil, _) => DoesNotMatch
  | (Cons(dp1, dp2), Cons(d1, d2)) =>
    switch (matches(dp1, d1)) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | (Cons(dp1, dp2), Cast(d, List(ty1), List(ty2))) =>
    matches_cast_Cons(dp1, dp2, d, [(ty1, ty2)])
  | (Cons(_, _), Cast(d, Hole, List(_))) => matches(dp, d)
  | (Cons(_, _), Cast(d, List(_), Hole)) => matches(dp, d)
  | (Cons(_, _), _) => DoesNotMatch
  | (Ap(_, _), _) => DoesNotMatch
  }
and matches_cast_Inj =
    (
      side: InjSide.t,
      dp: DHPat.t,
      d: DHExp.t,
      casts: list((HTyp.t, HTyp.t, HTyp.t, HTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Inj(_, side', d') =>
    switch (side, side') {
    | (L, L)
    | (R, R) =>
      let side_casts =
        List.map(
          (c: (HTyp.t, HTyp.t, HTyp.t, HTyp.t)) => {
            let (tyL1, tyR1, tyL2, tyR2) = c;
            switch (side) {
            | L => (tyL1, tyL2)
            | R => (tyR1, tyR2)
            };
          },
          casts,
        );
      matches(dp, DHExp.apply_casts(d', side_casts));
    | _ => DoesNotMatch
    }
  | Cast(d', Sum(tyL1, tyR1), Sum(tyL2, tyR2)) =>
    matches_cast_Inj(side, dp, d', [(tyL1, tyR1, tyL2, tyR2), ...casts])
  | Cast(d', Sum(_, _), Hole)
  | Cast(d', Hole, Sum(_, _)) => matches_cast_Inj(side, dp, d', casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _) => Indet
  | InvalidText(_) => Indet
  | ExpandingKeyword(_, _, _) => Indet
  | Let(_, _, _) => Indet
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, Fun(_)) => DoesNotMatch
  | Closure(_, _) => Indet
  | Ap(_, _) => Indet
  | ApBuiltin(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | ListNil(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Pair(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => Indet
  | EmptyHole(_, _) => Indet
  | NonEmptyHole(_, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  }
and matches_cast_Pair =
    (
      dp1: DHPat.t,
      dp2: DHPat.t,
      d: DHExp.t,
      left_casts: list((HTyp.t, HTyp.t)),
      right_casts: list((HTyp.t, HTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Pair(d1, d2) =>
    switch (matches(dp1, DHExp.apply_casts(d1, left_casts))) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
    | Matches(env1) =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | Cast(d', Prod([]), Prod([])) =>
    matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
  | Cast(d', Prod([head1, ...tail1]), Prod([head2, ...tail2])) =>
    matches_cast_Pair(
      dp1,
      dp2,
      d',
      [(head1, head2), ...left_casts],
      List.combine(tail1, tail2) @ right_casts,
    )
  | Cast(d', Prod(_), Hole)
  | Cast(d', Hole, Prod(_)) =>
    matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _) => Indet
  | InvalidText(_) => Indet
  | ExpandingKeyword(_, _, _) => Indet
  | Let(_, _, _) => Indet
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, Fun(_)) => DoesNotMatch
  | Closure(_, _) => Indet
  | Ap(_, _) => Indet
  | ApBuiltin(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | Inj(_, _, _) => DoesNotMatch
  | ListNil(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => Indet
  | EmptyHole(_, _) => Indet
  | NonEmptyHole(_, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  }
and matches_cast_Cons =
    (
      dp1: DHPat.t,
      dp2: DHPat.t,
      d: DHExp.t,
      elt_casts: list((HTyp.t, HTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Cons(d1, d2) =>
    switch (matches(dp1, DHExp.apply_casts(d1, elt_casts))) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      let list_casts =
        List.map(
          (c: (HTyp.t, HTyp.t)) => {
            let (ty1, ty2) = c;
            (HTyp.List(ty1), HTyp.List(ty2));
          },
          elt_casts,
        );
      switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      };
    | Matches(env1) =>
      let list_casts =
        List.map(
          (c: (HTyp.t, HTyp.t)) => {
            let (ty1, ty2) = c;
            (HTyp.List(ty1), HTyp.List(ty2));
          },
          elt_casts,
        );
      switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      };
    }
  | Cast(d', List(ty1), List(ty2)) =>
    matches_cast_Cons(dp1, dp2, d', [(ty1, ty2), ...elt_casts])
  | Cast(d', List(_), Hole) => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Cast(d', Hole, List(_)) => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _) => Indet
  | InvalidText(_) => Indet
  | ExpandingKeyword(_, _, _) => Indet
  | Let(_, _, _) => Indet
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, d') => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Ap(_, _) => Indet
  | ApBuiltin(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | Inj(_, _, _) => DoesNotMatch
  | ListNil(_) => DoesNotMatch
  | Pair(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => Indet
  | EmptyHole(_, _) => Indet
  | NonEmptyHole(_, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  };

/* closed substitution [d1/x]d2
   Not needed for evaluation with environments,
   leaving in case it's useful for something else */
let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t =>
  switch (d2) {
  | BoundVar(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | FreeVar(_) => d2
  | InvalidText(_) => d2
  | ExpandingKeyword(_) => d2
  | Let(dp, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 =
      if (DHPat.binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    Let(dp, d3, d4);
  | FixF(y, ty, d3) =>
    let d3 =
      if (Var.eq(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    FixF(y, ty, d3);
  | Fun(dp, ty, d3) =>
    if (DHPat.binds_var(x, dp)) {
      d2;
    } else {
      let d3 = subst_var(d1, x, d3);
      Fun(dp, ty, d3);
    }
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' = subst_var_env(d1, x, env);
    let d3' = subst_var(d1, x, d3);
    Closure(env', d3');
  | Ap(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Ap(d3, d4);
  | ApBuiltin(ident, args) =>
    let args = List.map(subst_var(d1, x), args);
    ApBuiltin(ident, args);
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => d2
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Cons(d3, d4);
  | BinBoolOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinBoolOp(op, d3, d4);
  | BinIntOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinIntOp(op, d3, d4);
  | BinFloatOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinFloatOp(op, d3, d4);
  | Inj(ty, side, d3) =>
    let d3 = subst_var(d1, x, d3);
    Inj(ty, side, d3);
  | Pair(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Pair(d3, d4);
  | ConsistentCase(Case(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    ConsistentCase(Case(d3, rules, n));
  | InconsistentBranches(u, i, Case(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    InconsistentBranches(u, i, Case(d3, rules, n));
  | EmptyHole(u, i) => EmptyHole(u, i)
  | NonEmptyHole(reason, u, i, d3) =>
    let d3' = subst_var(d1, x, d3);
    NonEmptyHole(reason, u, i, d3');
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    Cast(d', ty1, ty2);
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    FailedCast(d', ty1, ty2);
  | InvalidOperation(d, err) =>
    let d' = subst_var(d1, x, d);
    InvalidOperation(d', err);
  }

and subst_var_rules =
    (d1: DHExp.t, x: Var.t, rules: list(DHExp.rule)): list(DHExp.rule) =>
  rules
  |> List.map((r: DHExp.rule) =>
       switch (r) {
       | Rule(dp, d2) =>
         if (DHPat.binds_var(x, dp)) {
           r;
         } else {
           Rule(dp, subst_var(d1, x, d2));
         }
       }
     )

and subst_var_env = (d1: DHExp.t, x: Var.t, sigma: EvalEnv.t): EvalEnv.t =>
  EvalEnv.map_keep_id(
    (_, dr) =>
      switch (dr) {
      | BoxedValue(d) => BoxedValue(subst_var(d1, x, d))
      | Indet(d) => Indet(subst_var(d1, x, d))
      },
    sigma,
  );

let subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
  env
  |> List.fold_left(
       (d2, xd: (Var.t, DHExp.t)) => {
         let (x, d1) = xd;
         subst_var(d1, x, d2);
       },
       d,
     );

let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
  switch (op) {
  | And => BoolLit(b1 && b2)
  | Or => BoolLit(b1 || b2)
  };

let eval_bin_bool_op_short_circuit =
    (op: DHExp.BinBoolOp.t, b1: bool): option(EvaluatorResult.t) =>
  switch (op, b1) {
  | (Or, true) => Some(BoxedValue(BoolLit(true)))
  | (And, false) => Some(BoxedValue(BoolLit(false)))
  | _ => None
  };

let eval_bin_int_op = (op: DHExp.BinIntOp.t, n1: int, n2: int): DHExp.t => {
  switch (op) {
  | Minus => IntLit(n1 - n2)
  | Plus => IntLit(n1 + n2)
  | Times => IntLit(n1 * n2)
  | Divide => IntLit(n1 / n2)
  | LessThan => BoolLit(n1 < n2)
  | GreaterThan => BoolLit(n1 > n2)
  | Equals => BoolLit(n1 == n2)
  };
};

let eval_bin_float_op =
    (op: DHExp.BinFloatOp.t, f1: float, f2: float): DHExp.t => {
  switch (op) {
  | FPlus => FloatLit(f1 +. f2)
  | FMinus => FloatLit(f1 -. f2)
  | FTimes => FloatLit(f1 *. f2)
  | FDivide => FloatLit(f1 /. f2)
  | FLessThan => BoolLit(f1 < f2)
  | FGreaterThan => BoolLit(f1 > f2)
  | FEquals => BoolLit(f1 == f2)
  };
};

let rec evaluate =
        (es: EvalState.t, env: EvalEnv.t, d: DHExp.t)
        : (EvalState.t, EvaluatorResult.t) => {
  /* Increment number of evaluation steps (calls to `evaluate`). */
  let es = es |> EvalState.inc_eval_steps;

  switch (d) {
  | BoundVar(x) =>
    let dr =
      x
      |> EvalEnv.lookup(env)
      |> OptUtil.get(_ =>
           raise(EvaluatorError.Exception(FreeInvalidVar(x)))
         );
    switch (dr) {
    | BoxedValue(FixF(_) as d) => evaluate(es, env, d)
    | _ => (es, dr)
    };
  | Let(dp, d1, d2) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(d1))
    | (es, Indet(d1)) =>
      switch (matches(dp, d1)) {
      | Indet
      | DoesNotMatch => (es, Indet(Closure(env, Let(dp, d1, d2))))
      | Matches(env') =>
        let (es, env) = extend_evalenv_with_env(es, env', env);
        evaluate(es, env, d2);
      }
    }
  | FixF(f, ty, d) =>
    switch (evaluate(es, env, d)) {
    | (es, BoxedValue(Closure(env', Fun(_) as d''') as d'')) =>
      let (es, env'') =
        EvalEnv.extend(es, env', (f, BoxedValue(FixF(f, ty, d''))));
      (es, BoxedValue(Closure(env'', d''')));
    | _ => raise(EvaluatorError.Exception(EvaluatorError.FixFWithoutLambda))
    }
  | Fun(_) => (es, BoxedValue(Closure(env, d)))
  | Ap(d1, d2) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(Closure(closure_env, Fun(dp, _, d3)) as d1)) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(d2))
      | (es, Indet(d2)) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch
        | Indet => (es, Indet(Ap(d1, d2)))
        | Matches(env') =>
          // evaluate a closure: extend the closure environment with the
          // new bindings introduced by the function application.
          let (es, env) = extend_evalenv_with_env(es, env', closure_env);
          evaluate(es, env, d3);
        }
      }
    | (es, BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))))
    | (es, Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(d2'))
      | (es, Indet(d2')) =>
        /* ap cast rule */
        evaluate(es, env, Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')))
    | (es, Indet(d1')) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(d2'))
      | (es, Indet(d2')) => (es, Indet(Ap(d1', d2')))
      }
    }
  | ApBuiltin(ident, args) => evaluate_ap_builtin(es, env, ident, args)
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => (es, BoxedValue(d))
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(BoolLit(b1) as d1')) =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => (es, b3)
      | None =>
        switch (evaluate(es, env, d2)) {
        | (es, BoxedValue(BoolLit(b2))) => (
            es,
            BoxedValue(eval_bin_bool_op(op, b1, b2)),
          )
        | (_, BoxedValue(d2')) =>
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
        | (es, Indet(d2')) => (es, Indet(BinBoolOp(op, d1', d2')))
        }
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
    | (es, Indet(d1')) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(d2'))
      | (es, Indet(d2')) => (es, Indet(BinBoolOp(op, d1', d2')))
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(IntLit(n1) as d1')) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(IntLit(n2))) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) => (
            es,
            Indet(
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              ),
            ),
          )
        | _ => (es, BoxedValue(eval_bin_int_op(op, n1, n2)))
        }
      | (_, BoxedValue(d2')) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
      | (es, Indet(d2')) => (es, Indet(BinIntOp(op, d1', d2')))
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
    | (es, Indet(d1')) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(d2'))
      | (es, Indet(d2')) => (es, Indet(BinIntOp(op, d1', d2')))
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(FloatLit(f1) as d1')) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(FloatLit(f2))) => (
          es,
          BoxedValue(eval_bin_float_op(op, f1, f2)),
        )
      | (_, BoxedValue(d2')) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
      | (es, Indet(d2')) => (es, Indet(BinFloatOp(op, d1', d2')))
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
    | (es, Indet(d1')) =>
      switch (evaluate(es, env, d2)) {
      | (es, BoxedValue(d2'))
      | (es, Indet(d2')) => (es, Indet(BinFloatOp(op, d1', d2')))
      }
    }
  | Inj(ty, side, d1) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(d1')) => (es, BoxedValue(Inj(ty, side, d1')))
    | (es, Indet(d1')) => (es, Indet(Inj(ty, side, d1')))
    }
  | Pair(d1, d2) =>
    let (es, d1') = evaluate(es, env, d1);
    let (es, d2') = evaluate(es, env, d2);
    switch (d1', d2') {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => (es, Indet(Pair(d1, d2)))
    | (BoxedValue(d1), BoxedValue(d2)) => (es, BoxedValue(Pair(d1, d2)))
    };
  | Cons(d1, d2) =>
    let (es, d1') = evaluate(es, env, d1);
    let (es, d2') = evaluate(es, env, d2);
    switch (d1', d2') {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => (es, Indet(Cons(d1, d2)))
    | (BoxedValue(d1), BoxedValue(d2)) => (es, BoxedValue(Cons(d1, d2)))
    };
  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case(es, env, None, d1, rules, n)

  /* Generalized closures evaluate to themselves. Only
     lambda closures are BoxedValues; other closures are all Indet. */
  | Closure(_, d') =>
    switch (d') {
    | Fun(_) => (es, BoxedValue(d))
    | _ => (es, Indet(d))
    }

  /* Hole expressions */
  | InconsistentBranches(u, i, Case(d1, rules, n)) =>
    evaluate_case(es, env, Some((u, i)), d1, rules, n)
  | EmptyHole(u, i) => (es, Indet(Closure(env, EmptyHole(u, i))))
  | NonEmptyHole(reason, u, i, d1) =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(d1'))
    | (es, Indet(d1')) => (
        es,
        Indet(Closure(env, NonEmptyHole(reason, u, i, d1'))),
      )
    }
  | FreeVar(u, i, x) => (es, Indet(Closure(env, FreeVar(u, i, x))))
  | ExpandingKeyword(u, i, kw) => (
      es,
      Indet(Closure(env, ExpandingKeyword(u, i, kw))),
    )
  | InvalidText(u, i, text) => (
      es,
      Indet(Closure(env, InvalidText(u, i, text))),
    )

  /* Cast calculus */
  | Cast(d1, ty, ty') =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(d1') as result) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (es, result)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (es, result)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (es, BoxedValue(Cast(d1', ty, ty')))
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (es, BoxedValue(d1''));
          } else {
            (es, Indet(FailedCast(d1', ty, ty')));
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(es, env, d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(es, env, d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        (es, BoxedValue(Cast(d1', ty, ty')))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (es, result);
        } else {
          (es, BoxedValue(Cast(d1', ty, ty')));
        }
      }
    | (es, Indet(d1') as result) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (es, result)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (es, result)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (es, Indet(Cast(d1', ty, ty')))
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (es, Indet(d1''));
          } else {
            (es, Indet(FailedCast(d1', ty, ty')));
          }
        | _ => (es, Indet(Cast(d1', ty, ty')))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(es, env, d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(es, env, d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        (es, Indet(Cast(d1', ty, ty')))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (es, result);
        } else {
          (es, Indet(Cast(d1', ty, ty')));
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (evaluate(es, env, d1)) {
    | (es, BoxedValue(d1'))
    | (es, Indet(d1')) => (es, Indet(FailedCast(d1', ty, ty')))
    }
  | InvalidOperation(d, err) => (es, Indet(InvalidOperation(d, err)))
  };
}
and evaluate_case =
    (
      es: EvalState.t,
      env: EvalEnv.t,
      inconsistent_info: option(HoleClosure.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : (EvalState.t, EvaluatorResult.t) =>
  switch (evaluate(es, env, scrut)) {
  | (es, BoxedValue(scrut))
  | (es, Indet(scrut)) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      (
        es,
        switch (inconsistent_info) {
        | None => Indet(Closure(env, ConsistentCase(case)))
        | Some((u, i)) =>
          Indet(Closure(env, InconsistentBranches(u, i, case)))
        },
      );
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        (
          es,
          switch (inconsistent_info) {
          | None => Indet(Closure(env, ConsistentCase(case)))
          | Some((u, i)) =>
            Indet(Closure(env, InconsistentBranches(u, i, case)))
          },
        );
      | Matches(env') =>
        // extend environment with new bindings introduced
        let (es, env) = extend_evalenv_with_env(es, env', env);
        evaluate(es, env, d);
      // by the rule and evaluate the expression.
      | DoesNotMatch =>
        evaluate_case(
          es,
          env,
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
        )
      }
    }
  }

/* This function extends an EvalEnv.t with new bindings
   (an Environment.t from match()). We need to wrap the new bindings
   in a final judgment (BoxedValue or Indet), so we call evaluate()
   on it again, but it shouldn't change the value of the expression. */
and extend_evalenv_with_env =
    (es: EvalState.t, new_bindings: Environment.t, to_extend: EvalEnv.t)
    : (EvalState.t, EvalEnv.t) => {
  let (es, ei) = es |> EvalState.next_env_id;
  let result_map =
    List.fold_left(
      (new_env, (x, d)) => {
        /* The value of environment doesn't matter here */
        let (_, dr) = evaluate(es, EvalEnv.placeholder, d);
        VarBstMap.add(x, dr, new_env);
      },
      EvalEnv.result_map_of_evalenv(to_extend),
      new_bindings,
    );
  (es, (ei, result_map));
}

/* Evaluate the application of a built-in function. */
and evaluate_ap_builtin =
    (es: EvalState.t, env: EvalEnv.t, ident: string, args: list(DHExp.t))
    : (EvalState.t, EvaluatorResult.t) => {
  switch (Builtins.lookup_form(ident)) {
  | Some((eval, _)) => eval(es, env, args, evaluate)
  | None => raise(EvaluatorError.Exception(InvalidBuiltin(ident)))
  };
};
