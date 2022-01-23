[@deriving sexp]
type result = EvalEnv.result;

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
  | (_, EmptyHole(_, _, _)) => Indet
  | (_, NonEmptyHole(_, _, _, _, _)) => Indet
  | (_, FailedCast(_, _, _)) => Indet
  | (_, InvalidOperation(_)) => Indet
  | (_, FreeVar(_, _, _, _)) => Indet
  | (_, InvalidText(_)) => Indet
  | (_, Let(_, _, _)) => Indet
  | (_, Lam(_, _, _)) => DoesNotMatch
  | (_, Ap(_, _)) => Indet
  | (_, BinBoolOp(_, _, _)) => Indet
  | (_, BinIntOp(_, _, _)) => Indet
  | (_, BinFloatOp(_, _, _)) => Indet
  | (_, ConsistentCase(Case(_, _, _))) => Indet
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
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | Lam(_, _, _) => DoesNotMatch
  | Closure(_, _, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
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
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
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
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | Lam(_, _, _) => DoesNotMatch
  | Closure(_, _, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
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
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
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
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | Lam(_, _, _) => DoesNotMatch
  | Closure(_, _, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
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
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  };

/* closed substitution [d1/x]d2
   Not needed for evaluation with environments,
   leaving in case it's useful for something else */
/* let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t =>
     switch (d2) {
     | BoundVar(y) =>
       if (Var.eq(x, y)) {
         d1;
       } else {
         d2;
       }
     | FreeVar(_) => d2
     | InvalidText(_) => d2
     | Keyword(_) => d2
     | Let(dp, d3, d4) =>
       let d3 = subst_var(d1, x, d3);
       let d4 =
         if (DHPat.binds_var(x, dp)) {
           d4;
         } else {
           subst_var(d1, x, d4);
         };
       Let(dp, d3, d4);
     | Lam(dp, ty, d3)
     | Closure(_, dp, ty, d3) =>
       if (DHPat.binds_var(x, dp)) {
         d2;
       } else {
         let d3 = subst_var(d1, x, d3);
         Lam(dp, ty, d3);
       }
     | Ap(d3, d4) =>
       let d3 = subst_var(d1, x, d3);
       let d4 = subst_var(d1, x, d4);
       Ap(d3, d4);
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
     | InconsistentBranches(u, i, sigma, Case(d3, rules, n)) =>
       let d3 = subst_var(d1, x, d3);
       let rules = subst_var_rules(d1, x, rules);
       let sigma' = subst_var_env(d1, x, sigma);
       InconsistentBranches(u, i, sigma', Case(d3, rules, n));
     | EmptyHole(u, i, sigma) =>
       let sigma' = subst_var_env(d1, x, sigma);
       EmptyHole(u, i, sigma');
     | NonEmptyHole(reason, u, i, sigma, d3) =>
       let d3' = subst_var(d1, x, d3);
       let sigma' = subst_var_env(d1, x, sigma);
       NonEmptyHole(reason, u, i, sigma', d3');
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
       (d1: DHExp.t, x: Var.t, sigma: Environment.t): Environment.t =>
     sigma
     |> List.map(xd => {
          let (y, d) = xd;
          (y, subst_var(d1, x, d));
        });

   let subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
     env
     |> List.fold_left(
          (d2, xd: (Var.t, DHExp.t)) => {
            let (x, d1) = xd;
            subst_var(d1, x, d2);
          },
          d,
        ); */

/* expand_closures_to_lambdas recursively performs substitution
   on the bodies of closures throughout the result (including
   in hole environments)

   env is the current environment carried by an enclosing closure.
   It is only extended when examining a closure expression
   in the result. The expanding process begins by calling
   expand_closures_to_lambdas on an empty closure environment
   and the top-level DHExp

   expand_closures_in_holes is a helper that recurses through
   hole environments

   TODO: need to memoize environments (to help solve the performance
   issue)
   TODO: rename this (and associated functions)
   */
let rec subst_vars_within_lambdas =
        (hcc: HoleClosureCtx.t, env: EvalEnv.t, d: DHExp.t)
        : (HoleClosureCtx.t, DHExp.t) => {
  switch (d) {
  /* Bound variables should be looked up within the closure
     environment. If lookup fails, then variable is not bound. */
  | BoundVar(x) =>
    switch (EvalEnv.lookup(env, x)) {
    | Some(Indet(d'))
    | Some(BoxedValue(d')) => (hcc, d')
    | None => (hcc, d)
    }

  /* Non-hole expressions: expand recursively */
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (hcc, d)
  | Let(dp, d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, Let(dp, d1', d2'));
  | Lam(dp, ty, d') =>
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env, d');
    (hcc, Lam(dp, ty, d''));
  | Ap(d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, Ap(d1', d2'));
  | BinBoolOp(op, d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, BinBoolOp(op, d1', d2'));
  | BinIntOp(op, d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, BinIntOp(op, d1', d2'));
  | BinFloatOp(op, d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, BinFloatOp(op, d1', d2'));
  | Cons(d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, Cons(d1', d2'));
  | Inj(ty, side, d') =>
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env, d');
    (hcc, Inj(ty, side, d''));
  | Pair(d1, d2) =>
    let (hcc, d1') = subst_vars_within_lambdas(hcc, env, d1);
    let (hcc, d2') = subst_vars_within_lambdas(hcc, env, d2);
    (hcc, Pair(d1', d2'));
  | Cast(d', ty1, ty2) =>
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env, d');
    (hcc, Cast(d'', ty1, ty2));
  | FailedCast(d', ty1, ty2) =>
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env, d');
    (hcc, FailedCast(d'', ty1, ty2));
  | InvalidOperation(d', reason) =>
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env, d');
    (hcc, InvalidOperation(d'', reason));
  | ConsistentCase(_) => /* TODO */ (hcc, d)

  /* This shouldn't occur within a lambda body. */
  /* TODO: move this exception elsewhere. */
  | Closure(_) =>
    exception ClosureInsideLambdaBody;
    raise(ClosureInsideLambdaBody);

  /* Hole expressions:
     - Use the closure environment as the hole environment.
     - Number the hole closure appropriately.
     - Recurse through inner expression (if any). */
  | EmptyHole(u, _, _) =>
    /* Check if this hole closure already exists in hcc */
    switch (HoleClosureCtx.mem_hole_closure(hcc, u, env)) {
    | Some((ci, env)) => (hcc, EmptyHole(u, ci, env))
    | None =>
      let (hcc, ci) = HoleClosureCtx.get_hole_closure_id(hcc, u, env);
      (hcc, EmptyHole(u, ci, env));
    }
  | NonEmptyHole(reason, u, _, _, d') =>
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env, d');
    switch (HoleClosureCtx.mem_hole_closure(hcc, u, env)) {
    | Some((ci, env)) => (hcc, NonEmptyHole(reason, u, ci, env, d''))
    | None =>
      let (hcc, ci) = HoleClosureCtx.get_hole_closure_id(hcc, u, env);
      (hcc, NonEmptyHole(reason, u, ci, env, d''));
    };
  | InconsistentBranches(_) => /* TODO */ (hcc, d)
  };
}

/* Perform substitution inside unevaluated expressions, and
   renumber holes. Recurses through hole environments.

   TODO: rename to encompass renumber operation */
and expand_closures_to_lambdas =
    (hcc: HoleClosureCtx.t, d: DHExp.t): (HoleClosureCtx.t, DHExp.t) =>
  switch (d) {
  /* Non-hole expressions: recurse through subexpressions */
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (hcc, d)
  | Let(dp, d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, Let(dp, d1', d2'));
  | Ap(d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, Ap(d1', d2'));
  | BinBoolOp(op, d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, BinBoolOp(op, d1', d2'));
  | BinIntOp(op, d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, BinIntOp(op, d1', d2'));
  | BinFloatOp(op, d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, BinFloatOp(op, d1', d2'));
  | Cons(d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, Cons(d1', d2'));
  | Inj(ty, side, d') =>
    let (hcc, d'') = expand_closures_to_lambdas(hcc, d');
    (hcc, Inj(ty, side, d''));
  | Pair(d1, d2) =>
    let (hcc, d1') = expand_closures_to_lambdas(hcc, d1);
    let (hcc, d2') = expand_closures_to_lambdas(hcc, d2);
    (hcc, Pair(d1', d2'));
  | Cast(d', ty1, ty2) =>
    let (hcc, d'') = expand_closures_to_lambdas(hcc, d');
    (hcc, Cast(d'', ty1, ty2));
  | FailedCast(d', ty1, ty2) =>
    let (hcc, d'') = expand_closures_to_lambdas(hcc, d');
    (hcc, FailedCast(d'', ty1, ty2));
  | ConsistentCase(_) => /* TODO */ (hcc, d)
  | InvalidOperation(d', reason) =>
    let (hcc, d'') = expand_closures_to_lambdas(hcc, d');
    (hcc, InvalidOperation(d'', reason));

  /* Bound variables should not appear outside holes or closures */
  | BoundVar(x) =>
    raise(EvaluatorError.Exception(EvaluatorError.FreeInvalidVar(x)))

  /* Lambda should not appear outside closure in evaluated result */
  /* TODO: move this exception somewhere else */
  | Lam(_) =>
    exception LambdaOutsideClosure;
    raise(LambdaOutsideClosure);

  /* Closure:
     - Fix environment recursively.
     - Body is recursively substituted with its environment. */
  | Closure(env', dp, ty, d') =>
    let (hcc, env'') = expand_closures_in_holes(hcc, env');
    let (hcc, d'') = subst_vars_within_lambdas(hcc, env'', d');
    (hcc, Lam(dp, ty, d''));

  /* Hole expressions:
     - Fix environment recursively.
     - Number the hole closure appropriately.
     - Recurse through subexpressions if applicable. */
  | EmptyHole(u, _, env) =>
    switch (HoleClosureCtx.mem_hole_closure(hcc, u, env)) {
    | Some((ci, env)) => (hcc, EmptyHole(u, ci, env))
    | None =>
      let (hcc, env) = expand_closures_in_holes(hcc, env);
      let (hcc, ci) = HoleClosureCtx.get_hole_closure_id(hcc, u, env);
      (hcc, EmptyHole(u, ci, env));
    }
  | NonEmptyHole(reason, u, _, env, d') =>
    let (hcc, d'') = expand_closures_to_lambdas(hcc, d');
    switch (HoleClosureCtx.mem_hole_closure(hcc, u, env)) {
    | Some((ci, env)) => (hcc, NonEmptyHole(reason, u, ci, env, d''))
    | None =>
      let (hcc, env) = expand_closures_in_holes(hcc, env);
      let (hcc, ci) = HoleClosureCtx.get_hole_closure_id(hcc, u, env);
      (hcc, NonEmptyHole(reason, u, ci, env, d''));
    };
  | InconsistentBranches(_) => /* TODO */ (hcc, d)
  }

/* Apply expand_closures_to_lambdas to each expression in sigma,
   threading hcc throughout. */
and expand_closures_in_holes =
    (hcc: HoleClosureCtx.t, sigma: EvalEnv.t): (HoleClosureCtx.t, EvalEnv.t) => {
  let ei =
    sigma
    |> EvalEnv.id_of_evalenv
    |> OptUtil.get(_ => raise(EvalEnv.InvalidEvalEnvType));
  let (hcc, result_map) =
    (hcc, [])
    |> List.fold_right(
         ((var, var_result: result), (hcc, result_map)) => {
           let (hcc, var_result: result) =
             switch (var_result) {
             | BoxedValue(d) =>
               let (hcc, d) = expand_closures_to_lambdas(hcc, d);
               (hcc, BoxedValue(d));
             | Indet(d) =>
               let (hcc, d) = expand_closures_to_lambdas(hcc, d);
               (hcc, Indet(d));
             };
           (hcc, [(var, var_result), ...result_map]);
         },
         sigma |> EvalEnv.result_map_of_evalenv,
       );
  (hcc, Env(ei, result_map));
};

let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
  switch (op) {
  | And => BoolLit(b1 && b2)
  | Or => BoolLit(b1 || b2)
  };

let eval_bin_bool_op_short_circuit =
    (op: DHExp.BinBoolOp.t, b1: bool): option(result) =>
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
        (ec: EvalEnvIdGen.t, env: EvalEnv.t, d: DHExp.t)
        : (EvalEnvIdGen.t, result) => {
  /* open Sexplib.Sexp;
     print_endline(
       "D: "
       ++ to_string(DHExp.sexp_of_t(d))
       ++ " ENV: "
       ++ to_string(EvalEnv.sexp_of_t(env)),
     ); */
  switch (d) {
  | BoundVar(x) => (
      ec,
      x
      |> EvalEnv.lookup(env)
      |> OptUtil.get(_ =>
           raise(EvaluatorError.Exception(FreeInvalidVar(x)))
         ),
    )
  | Let(dp, d1, d2) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(d1))
    | (ec, Indet(d1)) =>
      switch (matches(dp, d1)) {
      | Indet
      | DoesNotMatch =>
        switch (evaluate(ec, env, d2)) {
        | (ec, BoxedValue(d2'))
        | (ec, Indet(d2')) => (ec, Indet(d2'))
        }
      | Matches(env') =>
        let match_result_map = map_environment_to_result_map(ec, env, env');
        let (ec, env) = EvalEnv.union_with_env(ec, match_result_map, env);
        evaluate(ec, env, d2);
      }
    }
  | Lam(dp, ty, d) => (ec, BoxedValue(Closure(env, dp, ty, d)))
  | Closure(_) => (ec, BoxedValue(d))
  | Ap(d1, d2) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(Closure(closure_env, dp, _, d3))) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(d2))
      | (ec, Indet(d2)) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch => (ec, Indet(d))
        | Indet => (ec, Indet(d))
        | Matches(env') =>
          // evaluate a closure: extend the existing environment with the
          // closure environment and the new bindings introduced by the
          // function application.
          let match_result_map = map_environment_to_result_map(ec, env, env');
          let (ec, env) = EvalEnv.union(ec, closure_env, env);
          let (ec, env) = EvalEnv.union_with_env(ec, match_result_map, env);
          evaluate(ec, env, d3);
        }
      }
    | (ec, BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))))
    | (ec, Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(d2'))
      | (ec, Indet(d2')) =>
        /* ap cast rule */
        evaluate(ec, env, Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedLam(d1')))
    | (ec, Indet(d1')) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(d2'))
      | (ec, Indet(d2')) => (ec, Indet(Ap(d1', d2')))
      }
    }
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => (ec, BoxedValue(d))
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(BoolLit(b1) as d1')) =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => (ec, b3)
      | None =>
        switch (evaluate(ec, env, d2)) {
        | (ec, BoxedValue(BoolLit(b2))) => (
            ec,
            BoxedValue(eval_bin_bool_op(op, b1, b2)),
          )
        | (_, BoxedValue(d2')) =>
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
        | (ec, Indet(d2')) => (ec, Indet(BinBoolOp(op, d1', d2')))
        }
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
    | (ec, Indet(d1')) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(d2'))
      | (ec, Indet(d2')) => (ec, Indet(BinBoolOp(op, d1', d2')))
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(IntLit(n1) as d1')) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(IntLit(n2))) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) => (
            ec,
            Indet(
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              ),
            ),
          )
        | _ => (ec, BoxedValue(eval_bin_int_op(op, n1, n2)))
        }
      | (_, BoxedValue(d2')) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
      | (ec, Indet(d2')) => (ec, Indet(BinIntOp(op, d1', d2')))
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
    | (ec, Indet(d1')) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(d2'))
      | (ec, Indet(d2')) => (ec, Indet(BinIntOp(op, d1', d2')))
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(FloatLit(f1) as d1')) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(FloatLit(f2))) => (
          ec,
          BoxedValue(eval_bin_float_op(op, f1, f2)),
        )
      | (_, BoxedValue(d2')) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
      | (ec, Indet(d2')) => (ec, Indet(BinFloatOp(op, d1', d2')))
      }
    | (_, BoxedValue(d1')) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
    | (ec, Indet(d1')) =>
      switch (evaluate(ec, env, d2)) {
      | (ec, BoxedValue(d2'))
      | (ec, Indet(d2')) => (ec, Indet(BinFloatOp(op, d1', d2')))
      }
    }
  | Inj(ty, side, d1) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(d1')) => (ec, BoxedValue(Inj(ty, side, d1')))
    | (ec, Indet(d1')) => (ec, Indet(Inj(ty, side, d1')))
    }
  | Pair(d1, d2) =>
    let (ec, d1') = evaluate(ec, env, d1);
    let (ec, d2') = evaluate(ec, env, d2);
    switch (d1', d2') {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => (ec, Indet(Pair(d1, d2)))
    | (BoxedValue(d1), BoxedValue(d2)) => (ec, BoxedValue(Pair(d1, d2)))
    };
  | Cons(d1, d2) =>
    let (ec, d1') = evaluate(ec, env, d1);
    let (ec, d2') = evaluate(ec, env, d2);
    switch (d1', d2') {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => (ec, Indet(Cons(d1, d2)))
    | (BoxedValue(d1), BoxedValue(d2)) => (ec, BoxedValue(Cons(d1, d2)))
    };
  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case(ec, env, None, d1, rules, n)
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
    evaluate_case(ec, env, Some((u, i, sigma)), d1, rules, n)
  | EmptyHole(u, i, _) => (ec, Indet(EmptyHole(u, i, env)))
  | NonEmptyHole(reason, u, i, _, d1) =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(d1'))
    | (ec, Indet(d1')) => (ec, Indet(NonEmptyHole(reason, u, i, env, d1')))
    }
  | FreeVar(_)
  | Keyword(_)
  | InvalidText(_) => (ec, Indet(d))
  | Cast(d1, ty, ty') =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(d1') as result) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (ec, result)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (ec, result)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (ec, BoxedValue(Cast(d1', ty, ty')))
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (ec, BoxedValue(d1''));
          } else {
            (ec, Indet(FailedCast(d1', ty, ty')));
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(ec, env, d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(ec, env, d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        (ec, BoxedValue(Cast(d1', ty, ty')))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (ec, result);
        } else {
          (ec, BoxedValue(Cast(d1', ty, ty')));
        }
      }
    | (ec, Indet(d1') as result) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (ec, result)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (ec, result)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (ec, Indet(Cast(d1', ty, ty')))
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (ec, Indet(d1''));
          } else {
            (ec, Indet(FailedCast(d1', ty, ty')));
          }
        | _ => (ec, Indet(Cast(d1', ty, ty')))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(ec, env, d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(ec, env, d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        (ec, Indet(Cast(d1', ty, ty')))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (ec, result);
        } else {
          (ec, Indet(Cast(d1', ty, ty')));
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (evaluate(ec, env, d1)) {
    | (ec, BoxedValue(d1'))
    | (ec, Indet(d1')) => (ec, Indet(FailedCast(d1', ty, ty')))
    }
  | InvalidOperation(d, err) => (ec, Indet(InvalidOperation(d, err)))
  };
}
and evaluate_case =
    (
      ec: EvalEnvIdGen.t,
      env: EvalEnv.t,
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : (EvalEnvIdGen.t, result) =>
  switch (evaluate(ec, env, scrut)) {
  | (ec, BoxedValue(scrut))
  | (ec, Indet(scrut)) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      (
        ec,
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        },
      );
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        (
          ec,
          switch (inconsistent_info) {
          | None => Indet(ConsistentCase(case))
          | Some((u, i, sigma)) =>
            Indet(InconsistentBranches(u, i, sigma, case))
          },
        );
      | Matches(env') =>
        // extend environment with new bindings introduced
        // by the rule and evaluate the expression.
        let match_result_map = map_environment_to_result_map(ec, env, env');
        let (ec, env) = EvalEnv.union_with_env(ec, match_result_map, env);
        evaluate(ec, env, d);
      | DoesNotMatch =>
        evaluate_case(
          ec,
          env,
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
        )
      }
    }
  }
and map_environment_to_result_map =
    (ec: EvalEnvIdGen.t, env: EvalEnv.t, sigma: Environment.t)
    : VarMap.t_(result) =>
  /* This function is specifically for wrapping final results from
     pattern matching subexpressions in the result type. Basically, if we
     call evaluate() on final expressions and using the same environment
     that the entire matched expression was called on, then this should
     leave the final subexpressions unchanged and wrap them in a result
     type. This should also leave ec alone. If these assumptions are not
     met, then the hole environments may be changed. */
  Environment.map(
    ((_, d)) => {
      let (_, dr) = evaluate(ec, env, d);
      /* for testing only: make sure that the expression is unchanged,
         otherwise throw an exception

         TODO: remove */
      switch (dr) {
      | BoxedValue(d')
      | Indet(d') =>
        if (d != d') {
          exception TestException;
          raise(TestException);
        } else {
          ();
        }
      };
      dr;
    },
    sigma,
  );
