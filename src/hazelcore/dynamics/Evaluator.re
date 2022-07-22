open EvaluatorMonad;
open EvaluatorMonad.Syntax;

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
  | IndetMatch;

let rec matches = (dp: DHPat.t, d: DHExp.t): match_result =>
  switch (dp, d) {
  | (_, BoundVar(_)) => DoesNotMatch
  | (EmptyHole(_, _), _)
  | (NonEmptyHole(_, _, _, _), _) => IndetMatch
  | (Wild, _) => Matches(Environment.empty)
  | (ExpandingKeyword(_, _, _), _) => DoesNotMatch
  | (InvalidText(_), _) => IndetMatch
  | (Var(x), _) =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  | (_, EmptyHole(_, _)) => IndetMatch
  | (_, NonEmptyHole(_, _, _, _)) => IndetMatch
  | (_, FailedCast(_, _, _)) => IndetMatch
  | (_, InvalidOperation(_)) => IndetMatch
  | (_, FreeVar(_, _, _)) => IndetMatch
  | (_, InvalidText(_)) => IndetMatch
  | (_, Let(_, _, _)) => IndetMatch
  | (_, FixF(_, _, _)) => DoesNotMatch
  | (_, Fun(_, _, _)) => DoesNotMatch
  | (_, Ap(_, _)) => IndetMatch
  | (_, BinBoolOp(_, _, _)) => IndetMatch
  | (_, BinIntOp(_, _, _)) => IndetMatch
  | (_, BinFloatOp(_, _, _)) => IndetMatch
  | (_, ConsistentCase(Case(_, _, _))) => IndetMatch

  /* Closure should match like underlying expression */
  | (_, Closure(_, Fun(_))) => DoesNotMatch
  | (_, Closure(_, _)) => IndetMatch

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
    | IndetMatch =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch
      | Matches(_) => IndetMatch
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
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
    | IndetMatch =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch
      | Matches(_) => IndetMatch
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
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
  | FreeVar(_, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, Fun(_)) => DoesNotMatch
  | Closure(_, _) => IndetMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
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
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_, _) => IndetMatch
  | NonEmptyHole(_, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
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
    | IndetMatch =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch
      | Matches(_) => IndetMatch
      }
    | Matches(env1) =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
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
  | FreeVar(_, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, Fun(_)) => DoesNotMatch
  | Closure(_, _) => IndetMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
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
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_, _) => IndetMatch
  | NonEmptyHole(_, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
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
    | IndetMatch =>
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
      | IndetMatch
      | Matches(_) => IndetMatch
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
      | IndetMatch => IndetMatch
      | Matches(env2) => Matches(Environment.union(env1, env2))
      };
    }
  | Cast(d', List(ty1), List(ty2)) =>
    matches_cast_Cons(dp1, dp2, d', [(ty1, ty2), ...elt_casts])
  | Cast(d', List(_), Hole) => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Cast(d', Hole, List(_)) => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, d') => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
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
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_, _) => IndetMatch
  | NonEmptyHole(_, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
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

and subst_var_env =
    (d1: DHExp.t, x: Var.t, env: ClosureEnvironment.t): ClosureEnvironment.t =>
  env |> ClosureEnvironment.map_keep_id(((_, d)) => subst_var(d1, x, d));

let subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
  env
  |> Environment.fold(
       (xd: (Var.t, DHExp.t), d2) => {
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

type t('a) = EvaluatorMonad.t('a);

let rec evaluate: (ClosureEnvironment.t, DHExp.t) => t(EvaluatorResult.t) =
  (env, d) => {
    /* Increment number of evaluation steps (calls to `evaluate`). */
    let* () = take_step;

    switch (d) {
    | BoundVar(x) =>
      let d =
        x
        |> ClosureEnvironment.lookup(env)
        |> OptUtil.get(_ =>
             raise(EvaluatorError.Exception(FreeInvalidVar(x)))
           );
      evaluate(env, d);

    | Let(dp, d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(d1)
      | Indet(d1) =>
        switch (matches(dp, d1)) {
        | IndetMatch
        | DoesNotMatch => Indet(Closure(env, Let(dp, d1, d2))) |> return
        | Matches(env') =>
          let* env = evaluate_extend_env(env', env);
          evaluate(env, d2);
        }
      };

    | FixF(f, _, _) as d =>
      let* env' = with_eig(ClosureEnvironment.extend(env, (f, d)));
      evaluate(env', d);

    | Fun(_) => BoxedValue(Closure(env, d)) |> return

    | Ap(d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(Closure(closure_env, Fun(dp, _, d3)) as d1) =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2)
        | Indet(d2) =>
          switch (matches(dp, d2)) {
          | DoesNotMatch
          | IndetMatch => Indet(Ap(d1, d2)) |> return
          | Matches(env') =>
            // evaluate a closure: extend the closure environment with the
            // new bindings introduced by the function application.
            let* env = evaluate_extend_env(env', closure_env);
            evaluate(env, d3);
          }
        };
      | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
      | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') =>
          /* ap cast rule */
          evaluate(env, Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
        };
      | BoxedValue(d1') =>
        raise(EvaluatorError.Exception(InvalidBoxedFun(d1')))
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(Ap(d1', d2')) |> return
        };
      };

    | ApBuiltin(ident, args) => evaluate_ap_builtin(env, ident, args)

    | ListNil(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | Triv => BoxedValue(d) |> return

    | BinBoolOp(op, d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (eval_bin_bool_op_short_circuit(op, b1)) {
        | Some(b3) => b3 |> return
        | None =>
          let* r2 = evaluate(env, d2);
          switch (r2) {
          | BoxedValue(BoolLit(b2)) =>
            BoxedValue(eval_bin_bool_op(op, b1, b2)) |> return
          | BoxedValue(d2') =>
            raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
          | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
          };
        }
      | BoxedValue(d1') =>
        raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
        };
      };

    | BinIntOp(op, d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(IntLit(n1) as d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(IntLit(n2)) =>
          switch (op, n1, n2) {
          | (Divide, _, 0) =>
            Indet(
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              ),
            )
            |> return
          | _ => BoxedValue(eval_bin_int_op(op, n1, n2)) |> return
          }
        | BoxedValue(d2') =>
          raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
        | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
        };
      | BoxedValue(d1') =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
        };
      };

    | BinFloatOp(op, d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(FloatLit(f1) as d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(FloatLit(f2)) =>
          BoxedValue(eval_bin_float_op(op, f1, f2)) |> return
        | BoxedValue(d2') =>
          raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
        };
      | BoxedValue(d1') =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
        };
      };

    | Inj(ty, side, d1) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1')) |> return
      | Indet(d1') => Indet(Inj(ty, side, d1')) |> return
      };

    | Pair(d1, d2) =>
      let* d1' = evaluate(env, d1);
      let* d2' = evaluate(env, d2);
      switch (d1', d2') {
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2)) |> return
      | (BoxedValue(d1), BoxedValue(d2)) =>
        BoxedValue(Pair(d1, d2)) |> return
      };

    | Cons(d1, d2) =>
      let* d1' = evaluate(env, d1);
      let* d2' = evaluate(env, d2);
      switch (d1', d2') {
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2)) |> return
      | (BoxedValue(d1), BoxedValue(d2)) =>
        BoxedValue(Cons(d1, d2)) |> return
      };

    | ConsistentCase(Case(d1, rules, n)) =>
      evaluate_case(env, None, d1, rules, n)

    /* Generalized closures evaluate to themselves. Only
       lambda closures are BoxedValues; other closures are all Indet. */
    | Closure(_, d') =>
      switch (d') {
      | Fun(_) => BoxedValue(d) |> return
      | _ => Indet(d) |> return
      }

    /* Hole expressions */
    | InconsistentBranches(u, i, Case(d1, rules, n)) =>
      evaluate_case(env, Some((u, i)), d1, rules, n)

    | EmptyHole(u, i) => Indet(Closure(env, EmptyHole(u, i))) |> return

    | NonEmptyHole(reason, u, i, d1) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(d1')
      | Indet(d1') =>
        Indet(Closure(env, NonEmptyHole(reason, u, i, d1'))) |> return
      };

    | FreeVar(u, i, x) => Indet(Closure(env, FreeVar(u, i, x))) |> return

    | ExpandingKeyword(u, i, kw) =>
      Indet(Closure(env, ExpandingKeyword(u, i, kw))) |> return

    | InvalidText(u, i, text) =>
      Indet(Closure(env, InvalidText(u, i, text))) |> return

    /* Cast calculus */
    | Cast(d1, ty, ty') =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(d1') as result =>
        switch (ground_cases_of(ty), ground_cases_of(ty')) {
        | (Hole, Hole) => result |> return
        | (Ground, Ground) =>
          /* if two types are ground and consistent, then they are eq */
          result |> return
        | (Ground, Hole) =>
          /* can't remove the cast or do anything else here, so we're done */
          BoxedValue(Cast(d1', ty, ty')) |> return
        | (Hole, Ground) =>
          /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
          switch (d1') {
          | Cast(d1'', ty'', Hole) =>
            if (HTyp.eq(ty'', ty')) {
              BoxedValue(d1'') |> return;
            } else {
              Indet(FailedCast(d1', ty, ty')) |> return;
            }
          | _ => raise(EvaluatorError.Exception(CastBVHoleGround(d1')))
          }
        | (Hole, NotGroundOrHole(ty'_grounded)) =>
          /* ITExpand rule */
          let d' =
            DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
          evaluate(env, d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
          evaluate(env, d');
        | (Ground, NotGroundOrHole(_))
        | (NotGroundOrHole(_), Ground) =>
          /* can't do anything when casting between diseq, non-hole types */
          BoxedValue(Cast(d1', ty, ty')) |> return
        | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
          /* they might be eq in this case, so remove cast if so */
          if (HTyp.eq(ty, ty')) {
            result |> return;
          } else {
            BoxedValue(Cast(d1', ty, ty')) |> return;
          }
        }
      | Indet(d1') as result =>
        switch (ground_cases_of(ty), ground_cases_of(ty')) {
        | (Hole, Hole) => result |> return
        | (Ground, Ground) =>
          /* if two types are ground and consistent, then they are eq */
          result |> return
        | (Ground, Hole) =>
          /* can't remove the cast or do anything else here, so we're done */
          Indet(Cast(d1', ty, ty')) |> return
        | (Hole, Ground) =>
          switch (d1') {
          | Cast(d1'', ty'', Hole) =>
            if (HTyp.eq(ty'', ty')) {
              Indet(d1'') |> return;
            } else {
              Indet(FailedCast(d1', ty, ty')) |> return;
            }
          | _ => Indet(Cast(d1', ty, ty')) |> return
          }
        | (Hole, NotGroundOrHole(ty'_grounded)) =>
          /* ITExpand rule */
          let d' =
            DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
          evaluate(env, d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
          evaluate(env, d');
        | (Ground, NotGroundOrHole(_))
        | (NotGroundOrHole(_), Ground) =>
          /* can't do anything when casting between diseq, non-hole types */
          Indet(Cast(d1', ty, ty')) |> return
        | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
          /* it might be eq in this case, so remove cast if so */
          if (HTyp.eq(ty, ty')) {
            result |> return;
          } else {
            Indet(Cast(d1', ty, ty')) |> return;
          }
        }
      };

    | FailedCast(d1, ty, ty') =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(d1')
      | Indet(d1') => Indet(FailedCast(d1', ty, ty')) |> return
      };

    | InvalidOperation(d, err) => Indet(InvalidOperation(d, err)) |> return
    };
  }
and evaluate_case =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : t(EvaluatorResult.t) => {
  let* rscrut = evaluate(env, scrut);
  switch (rscrut) {
  | BoxedValue(scrut)
  | Indet(scrut) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      (
        switch (inconsistent_info) {
        | None => Indet(Closure(env, ConsistentCase(case)))
        | Some((u, i)) =>
          Indet(Closure(env, InconsistentBranches(u, i, case)))
        }
      )
      |> return;
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | IndetMatch =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        (
          switch (inconsistent_info) {
          | None => Indet(Closure(env, ConsistentCase(case)))
          | Some((u, i)) =>
            Indet(Closure(env, InconsistentBranches(u, i, case)))
          }
        )
        |> return;
      | Matches(env') =>
        // extend environment with new bindings introduced
        let* env = evaluate_extend_env(env', env);
        evaluate(env, d);
      // by the rule and evaluate the expression.
      | DoesNotMatch =>
        evaluate_case(
          env,
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
        )
      }
    }
  };
}

/* This function extends an ClosureEnvironment.t with new bindings
   (an Environment.t from match()). We need to wrap the new bindings
   in a final judgment (BoxedValue or Indet), so we call evaluate()
   on it again, but it shouldn't change the value of the expression. */
and evaluate_extend_env =
    (new_bindings: Environment.t, to_extend: ClosureEnvironment.t)
    : t(ClosureEnvironment.t) => {
  let map =
    new_bindings
    |> Environment.fold(
         ((x, d), new_env) => Environment.extend(new_env, (x, d)),
         ClosureEnvironment.map_of(to_extend),
       );

  with_eig(eig => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, map), eig);
  });
}

/* Evaluate the application of a built-in function. */
and evaluate_ap_builtin =
    (env: ClosureEnvironment.t, ident: string, args: list(DHExp.t))
    : t(EvaluatorResult.t) => {
  switch (Builtins.lookup_form(ident)) {
  | Some((eval, _)) => eval(env, args, evaluate)
  | None => raise(EvaluatorError.Exception(InvalidBuiltin(ident)))
  };
};

let evaluate = (env, d) => evaluate(env, d, EvaluatorState.init);
