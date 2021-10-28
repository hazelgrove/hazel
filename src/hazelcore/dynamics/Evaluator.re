open Sexplib.Std;

[@deriving sexp]
type result =
  | InvalidInput(int)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

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
  | (_, FixF(_, _, _)) => DoesNotMatch
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
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
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
  | Sequence(_)
  | AssertLit(_)
  | FailedAssert(_) => DoesNotMatch //TODO(andrew)
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
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
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
  | Sequence(_)
  | AssertLit(_)
  | FailedAssert(_) => DoesNotMatch //TODO(andrew)
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
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
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
  | Sequence(_)
  | AssertLit(_)
  | FailedAssert(_) => DoesNotMatch //TODO(andrew)
  };

/* closed substitution [d1/x]d2*/
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
  | FixF(y, ty, d3) =>
    let d3 =
      if (Var.eq(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    FixF(y, ty, d3);
  | Lam(dp, ty, d3) =>
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
  | Sequence(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Sequence(d3, d4);
  | AssertLit(n) => AssertLit(n)
  | FailedAssert(n, d3) =>
    let d3 = subst_var(d1, x, d3);
    FailedAssert(n, d3);
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
     );

let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
  switch (op) {
  | And => BoolLit(b1 && b2)
  | Or => BoolLit(b1 || b2)
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
        (d: DHExp.t, assert_map: AssertMap.t): (result, AssertMap.t) =>
  switch (d) {
  | BoundVar(_) => (InvalidInput(1), assert_map)
  | FailedAssert(_, d1) => (Indet(d1), assert_map)
  | AssertLit(_) => (BoxedValue(d), assert_map)
  | Let(dp, d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(d1), map1)
    | (Indet(d1), map1) =>
      switch (matches(dp, d1)) {
      | Indet => (Indet(d), map1)
      | DoesNotMatch => (Indet(d), map1)
      | Matches(env) => evaluate(subst(env, d2), map1)
      }
    }
  | FixF(x, _, d1) => evaluate(subst_var(d, x, d1), assert_map)
  | Lam(_, _, _) => (BoxedValue(d), assert_map)
  | Sequence(d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(_), map1) => evaluate(d2, map1)
    | (Indet(d1), map1) =>
      switch (evaluate(d2, map1)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), map1)
      | (BoxedValue(d2), map2) => (Indet(Sequence(d1, d2)), map2)
      | (Indet(d2), map2) => (Indet(Sequence(d1, d2)), map2)
      }
    }
  | Ap(d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(AssertLit(n)), _) =>
      switch (evaluate(d2, assert_map)) {
      | (BoxedValue(BoolLit(b)), _) =>
        b
          ? {
            let assert_result =
              AssertMap.extend((n, AssertResult.Pass), assert_map);
            (BoxedValue(Triv), assert_result);
          }
          : {
            let assert_result =
              AssertMap.extend((n, AssertResult.Fail), assert_map);
            (Indet(FailedAssert(n, d2)), assert_result);
          }
      | _ =>
        let assert_result =
          AssertMap.extend((n, AssertResult.Indet), assert_map);
        (Indet(Ap(AssertLit(n), d2)), assert_result);
      }
    | (BoxedValue(Lam(dp, _, d3)), map1) =>
      switch (evaluate(d2, map1)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), map1)
      | (BoxedValue(d2), map2)
      | (Indet(d2), map2) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch => (Indet(d), map2)
        | Indet => (Indet(d), map2)
        | Matches(env) =>
          /* beta rule */
          evaluate(subst(env, d3), map2)
        }
      }
    | (BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), _)
    | (Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(d2'), assert_map)
      | (Indet(d2'), assert_map) =>
        /* ap cast rule */
        evaluate(
          Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'),
          assert_map,
        )
      }
    | (BoxedValue(_), _) => (InvalidInput(2), assert_map)
    | (Indet(d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(d2'), _)
      | (Indet(d2'), _) => (Indet(Ap(d1', d2')), assert_map)
      }
    }
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => (BoxedValue(d), assert_map)
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(BoolLit(b1) as d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(BoolLit(b2)), _) => (
          BoxedValue(eval_bin_bool_op(op, b1, b2)),
          assert_map,
        )
      | (BoxedValue(_), _) => (InvalidInput(3), assert_map)
      | (Indet(d2'), _) => (Indet(BinBoolOp(op, d1', d2')), assert_map)
      }
    | (BoxedValue(_), _) => (InvalidInput(4), assert_map)
    | (Indet(d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(d2'), _)
      | (Indet(d2'), _) => (Indet(BinBoolOp(op, d1', d2')), assert_map)
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(IntLit(n1) as d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(IntLit(n2)), _) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) => (
            Indet(
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              ),
            ),
            assert_map,
          )
        | _ => (BoxedValue(eval_bin_int_op(op, n1, n2)), assert_map)
        }
      | (BoxedValue(_), _) => (InvalidInput(3), assert_map)
      | (Indet(d2'), _) => (Indet(BinIntOp(op, d1', d2')), assert_map)
      }
    | (BoxedValue(_), _) => (InvalidInput(4), assert_map)
    | (Indet(d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(d2'), _)
      | (Indet(d2'), _) => (Indet(BinIntOp(op, d1', d2')), assert_map)
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(FloatLit(f1) as d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(FloatLit(f2)), _) => (
          BoxedValue(eval_bin_float_op(op, f1, f2)),
          assert_map,
        )
      | (BoxedValue(_), _) => (InvalidInput(8), assert_map)
      | (Indet(d2'), _) => (Indet(BinFloatOp(op, d1', d2')), assert_map)
      }
    | (BoxedValue(_), _) => (InvalidInput(7), assert_map)
    | (Indet(d1'), _) =>
      switch (evaluate(d2, assert_map)) {
      | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
      | (BoxedValue(d2'), _)
      | (Indet(d2'), _) => (Indet(BinFloatOp(op, d1', d2')), assert_map)
      }
    }
  | Inj(ty, side, d1) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(d1'), map1) => (BoxedValue(Inj(ty, side, d1')), map1)
    | (Indet(d1'), map1) => (Indet(Inj(ty, side, d1')), map1)
    }
  | Pair(d1, d2) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (Indet(d1), map1) =>
      switch (evaluate(d2, map1)) {
      | (Indet(d2), map2)
      | (BoxedValue(d2), map2) => (Indet(Pair(d1, d2)), map2)
      | (InvalidInput(msg), _) => (InvalidInput(msg), map1)
      }
    | (BoxedValue(d1), map1) =>
      switch (evaluate(d2, map1)) {
      | (Indet(d2), map2) => (Indet(Pair(d1, d2)), map2)
      | (BoxedValue(d2), map2) => (BoxedValue(Pair(d1, d2)), map2)
      | (InvalidInput(msg), _) => (InvalidInput(msg), map1)
      }
    //maybe need to be sequetial?
    }
  /*| Pair(d1, d2) =>
    switch (evaluate(d1, assert_map), evaluate(d2, assert_map)) {
    //maybe need to be sequetial?
    | ((InvalidInput(msg), _), _)
    | (_, (InvalidInput(msg), _)) => (InvalidInput(msg), assert_map)
    | ((Indet(d1), map1), (Indet(d2), map2))
    | ((Indet(d1), map1), (BoxedValue(d2), map2))
    | ((BoxedValue(d1), _), (Indet(d2), _)) => (
        Indet(Pair(d1, d2)),
        assert_map,
      )
    | ((BoxedValue(d1), _), (BoxedValue(d2), _)) =>
      (BoxedValue(Pair(d1, d2)), assert_map);
    }*/
  | Cons(d1, d2) =>
    switch (evaluate(d1, assert_map), evaluate(d2, assert_map)) {
    | ((InvalidInput(msg), _), _)
    | (_, (InvalidInput(msg), _)) => (InvalidInput(msg), assert_map)
    | ((Indet(d1), _), (Indet(d2), _))
    | ((Indet(d1), _), (BoxedValue(d2), _))
    | ((BoxedValue(d1), _), (Indet(d2), _)) => (
        Indet(Cons(d1, d2)),
        assert_map,
      )
    | ((BoxedValue(d1), _), (BoxedValue(d2), _)) => (
        BoxedValue(Cons(d1, d2)),
        assert_map,
      )
    }
  | ConsistentCase(Case(d1, rules, n)) => (
      evaluate_case(None, d1, rules, n, assert_map),
      assert_map,
    )
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) => (
      evaluate_case(Some((u, i, sigma)), d1, rules, n, assert_map),
      assert_map,
    )
  | EmptyHole(_) => (Indet(d), assert_map)
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(d1'), _)
    | (Indet(d1'), _) => (
        Indet(NonEmptyHole(reason, u, i, sigma, d1')),
        assert_map,
      )
    }
  | FreeVar(_) => (Indet(d), assert_map)
  | Keyword(_) => (Indet(d), assert_map)
  | InvalidText(_) => (Indet(d), assert_map)
  | Cast(d1, ty, ty') =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(d1') as result, _) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (result, assert_map)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (result, assert_map)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (BoxedValue(Cast(d1', ty, ty')), assert_map)
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (BoxedValue(d1''), assert_map);
          } else {
            (Indet(FailedCast(d1', ty, ty')), assert_map);
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          (InvalidInput(6), assert_map)
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(d', assert_map);
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(d', assert_map);
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        (BoxedValue(Cast(d1', ty, ty')), assert_map)
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (result, assert_map);
        } else {
          (BoxedValue(Cast(d1', ty, ty')), assert_map);
        }
      }
    | (Indet(d1') as result, _) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (result, assert_map)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (result, assert_map)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (Indet(Cast(d1', ty, ty')), assert_map)
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (Indet(d1''), assert_map);
          } else {
            (Indet(FailedCast(d1', ty, ty')), assert_map);
          }
        | _ => (Indet(Cast(d1', ty, ty')), assert_map)
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(d', assert_map);
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(d', assert_map);
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        (Indet(Cast(d1', ty, ty')), assert_map)
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (result, assert_map);
        } else {
          (Indet(Cast(d1', ty, ty')), assert_map);
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (evaluate(d1, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(d1'), _)
    | (Indet(d1'), _) => (Indet(FailedCast(d1', ty, ty')), assert_map)
    }
  | InvalidOperation(d, err) =>
    switch (evaluate(d, assert_map)) {
    | (InvalidInput(msg), _) => (InvalidInput(msg), assert_map)
    | (BoxedValue(d'), _)
    | (Indet(d'), _) => (Indet(InvalidOperation(d', err)), assert_map)
    }
  }
and evaluate_case =
    (
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      assert_map: AssertMap.t,
    )
    : result =>
  switch (evaluate(scrut, assert_map)) {
  | (InvalidInput(msg), _) => InvalidInput(msg)
  | (BoxedValue(scrut), _)
  | (Indet(scrut), _) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      switch (inconsistent_info) {
      | None => Indet(ConsistentCase(case))
      | Some((u, i, sigma)) =>
        Indet(InconsistentBranches(u, i, sigma, case))
      };
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Matches(env) => fst(evaluate(subst(env, d), assert_map))
      | DoesNotMatch =>
        evaluate_case(
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
          assert_map,
        )
      }
    }
  };
