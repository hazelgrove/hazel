[@deriving sexp]
type invalid_input =
  //| OutOfFuel // 0
  | FreeOrInvalidVariable // 1
  | ApInvalidBoxedFunctionVal // 2
  | BoxedNotIntLit2 // 3
  | BoxedNotIntLit1 // 4
  //| BadPatternMatch // 5
  | CastBVHoleGround // 6
  | BoxedNotFloatLit1 // 7
  | BoxedNotFloatLit2; //8

[@deriving sexp]
type result =
  | InvalidInput(invalid_input)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

[@deriving sexp]
type state = {assert_map: AssertMap.t};

[@deriving sexp]
type report = (result, state);

let init_state = {assert_map: []};

let extend_assert_map = ({assert_map, _}: state, n, result): state => {
  assert_map: AssertMap.extend((n, result), assert_map),
};

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
  | FailedAssert(_) => DoesNotMatch
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
  | FailedAssert(_) => DoesNotMatch
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
  | FailedAssert(_) => DoesNotMatch
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

type eval_input = (DHExp.t, state);

let bind' =
    (x: report, ~boxed: eval_input => report, ~indet: eval_input => report)
    : report =>
  switch (x) {
  | (InvalidInput(_), _) as ii => ii
  | (BoxedValue(d'), state) => boxed((d', state))
  | (Indet(d'), state) => indet((d', state))
  };
let bind_both = (x: report, f: eval_input => report): report =>
  bind'(x, ~boxed=f, ~indet=f);

let rec evaluate = (~state: state=init_state, d: DHExp.t): report =>
  switch (d) {
  | BoolLit(_)
  | ListNil(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv
  | Lam(_, _, _)
  | AssertLit(_) => (BoxedValue(d), state)
  | FreeVar(_)
  | Keyword(_)
  | InvalidText(_)
  | EmptyHole(_) => (Indet(d), state)
  | BoundVar(_) => (InvalidInput(FreeOrInvalidVariable), state)
  | FailedAssert(_, d1) => (Indet(d1), state)
  | FixF(x, _, d1) => evaluate(subst_var(d, x, d1), ~state)
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let mk_none_empty = d1' => DHExp.NonEmptyHole(reason, u, i, sigma, d1');
    evalbind_indet((d1, state), mk_none_empty);
  | Inj(ty, side, d1) =>
    eval_unary_constructor((d1, state), d1' => DHExp.Inj(ty, side, d1'))
  | Pair(d1, d2) =>
    let mk_pair = (d1', d2') => DHExp.Pair(d1', d2');
    eval_binary_constructor((d1, state), d2, mk_pair);
  | Cons(d1, d2) =>
    let mk_cons = (d1', d2') => DHExp.Cons(d1', d2');
    eval_binary_constructor((d1, state), d2, mk_cons);
  | Let(dp, d1, d2) =>
    evalbind_both((d1, state), ((d1', state)) =>
      switch (matches(dp, d1')) {
      | Matches(env) => evaluate(subst(env, d2), ~state)
      | Indet
      | DoesNotMatch => (Indet(d), state)
      }
    )
  | Sequence(d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (InvalidInput(_), _) as ii => ii
    | (BoxedValue(_), state) => evaluate(d2, ~state)
    | (Indet(d1), state) =>
      evalbind_indet((d2, state), d2 => Sequence(d1, d2))
    }
  | Ap(d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (InvalidInput(_), _) as ii => ii
    | (BoxedValue(AssertLit(n)), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(BoolLit(true)), state) => (
          BoxedValue(Triv),
          extend_assert_map(state, n, AssertResult.Pass),
        )
      | (BoxedValue(BoolLit(false)), state) => (
          Indet(FailedAssert(n, d2)),
          extend_assert_map(state, n, AssertResult.Fail),
        )
      | _ => (
          Indet(Ap(AssertLit(n), d2)),
          extend_assert_map(state, n, AssertResult.Indet),
        )
      }
    | (BoxedValue(Lam(dp, _, d3)), state) =>
      evalbind_both((d2, state), ((d2, state)) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch
        | Indet => (Indet(d), state)
        | Matches(env) =>
          /* beta rule */
          evaluate(subst(env, d3), ~state)
        }
      )
    | (BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), state)
    | (Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), state) =>
      evalbind_both((d2, state), ((d2', state)) =>
        evaluate(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'), ~state)
      )
    | (BoxedValue(_), state) => (
        InvalidInput(ApInvalidBoxedFunctionVal),
        state,
      )
    | (Indet(d1'), state) =>
      evalbind_indet((d2, state), (d2') => (Ap(d1', d2'): DHExp.t))
    }
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (InvalidInput(_), _) as ii => ii
    | (BoxedValue(BoolLit(b1) as d1'), _) =>
      switch (evaluate(d2, ~state)) {
      | (InvalidInput(_), _) as ii => ii
      | (BoxedValue(BoolLit(b2)), state) => (
          BoxedValue(eval_bin_bool_op(op, b1, b2)),
          state,
        )
      | (BoxedValue(_), state) => (InvalidInput(BoxedNotIntLit2), state)
      | (Indet(d2'), state) => (Indet(BinBoolOp(op, d1', d2')), state)
      }
    | (BoxedValue(_), state) => (InvalidInput(BoxedNotIntLit1), state)
    | (Indet(d1'), state) =>
      evalbind_indet((d2, state), (d2') =>
        (BinBoolOp(op, d1', d2'): DHExp.t)
      )
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (InvalidInput(_), _) as ii => ii
    | (Indet(d1'), state) =>
      evalbind_indet((d2, state), d2' => BinIntOp(op, d1', d2'))
    | (BoxedValue(IntLit(n1) as d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (InvalidInput(_), _) as ii => ii
      | (BoxedValue(IntLit(n2)), state) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) => (
            Indet(
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              ),
            ),
            state,
          )
        | _ => (BoxedValue(eval_bin_int_op(op, n1, n2)), state)
        }
      | (BoxedValue(_), state) => (InvalidInput(BoxedNotIntLit2), state)
      | (Indet(d2'), state) => (Indet(BinIntOp(op, d1', d2')), state)
      }
    | (BoxedValue(_), state) => (InvalidInput(BoxedNotIntLit1), state)
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (InvalidInput(_), _) as ii => ii
    | (Indet(d1'), state) =>
      evalbind_indet((d2, state), d2 => BinFloatOp(op, d1', d2))
    | (BoxedValue(FloatLit(f1) as d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (InvalidInput(_), _) as ii => ii
      | (BoxedValue(FloatLit(f2)), state) => (
          BoxedValue(eval_bin_float_op(op, f1, f2)),
          state,
        )
      | (BoxedValue(_), state) => (InvalidInput(BoxedNotFloatLit2), state)
      | (Indet(d2'), state) => (Indet(BinFloatOp(op, d1', d2')), state)
      }
    | (BoxedValue(_), state) => (InvalidInput(BoxedNotFloatLit1), state)
    }
  | FailedCast(d1, ty, ty') =>
    evalbind_indet((d1, state), d1' => FailedCast(d1', ty, ty'))
  | InvalidOperation(d, err) =>
    evalbind_indet((d, state), d' => InvalidOperation(d', err))
  | ConsistentCase(Case(d1, rules, n)) => (
      evaluate_case(None, d1, rules, n, state),
      state,
    )
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) => (
      evaluate_case(Some((u, i, sigma)), d1, rules, n, state),
      state,
    )
  | Cast(d1, ty, ty') =>
    switch (evaluate(d1, ~state)) {
    | (InvalidInput(_), _) as ii => ii
    | (BoxedValue(boxed_val) as result, state) =>
      eval_cast_boxed(ty, ty', result, state, boxed_val)
    | (Indet(indet_val) as result, state) =>
      eval_cast_indet(ty, ty', result, state, indet_val)
    }
  }
and eval_unary_constructor = ((d1: DHExp.t, state: state), cons1) =>
  switch (evaluate(d1, ~state)) {
  | (InvalidInput(_), _) as ii => ii
  | (Indet(d1'), map1) => (Indet(cons1(d1')), map1)
  | (BoxedValue(d1'), map1) => (BoxedValue(cons1(d1')), map1)
  }
and eval_unary_constructor' =
    (cons, d2: DHExp.t, (d1': DHExp.t, state: state)) =>
  eval_unary_constructor((d2, state), d2' => cons(d1', d2'))
and evalbind_indet' = (cons, d2: DHExp.t, (d1', state)) =>
  evalbind_indet((d2, state), d2' => cons(d1', d2'))
and eval_binary_constructor = ((d1, state: state), d2, cons) =>
  evalbind_sep(
    (d1, state),
    ~indet=evalbind_indet'(cons, d2),
    ~boxed=eval_unary_constructor'(cons, d2),
  )
and evalbind_both =
    ((d: DHExp.t, state: state), f: eval_input => report): report =>
  bind_both(evaluate(d, ~state), f)
and evalbind_stateless =
    ((d: DHExp.t, state: state), f: DHExp.t => result): report =>
  evalbind_both((d, state), ((d', state)) => (f(d'), state))
and evalbind_indet =
    ((d: DHExp.t, state: state), f: DHExp.t => DHExp.t): report =>
  evalbind_stateless((d, state), d' => Indet(f(d')))
and evalbind_sep =
    (
      (d: DHExp.t, state: state),
      ~boxed: eval_input => report,
      ~indet: eval_input => report,
    )
    : report =>
  bind'(evaluate(d, ~state), ~boxed, ~indet)
and evaluate_case =
    (
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      state: state,
    )
    : result =>
  switch (evaluate(scrut, ~state)) {
  | (InvalidInput(msg), _) => InvalidInput(msg)
  | (BoxedValue(scrut), _)
  | (Indet(scrut), _) =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    let none_or_indet: result =
      switch (inconsistent_info) {
      | None => Indet(ConsistentCase(case))
      | Some((u, i, sigma)) =>
        Indet(InconsistentBranches(u, i, sigma, case))
      };
    switch (List.nth_opt(rules, current_rule_index)) {
    | None => none_or_indet
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | Indet => none_or_indet
      | Matches(env) => fst(evaluate(subst(env, d), ~state))
      | DoesNotMatch =>
        evaluate_case(
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
          state,
        )
      }
    };
  }
and eval_cast_boxed = (ty, ty', result, state, d1') =>
  switch (ground_cases_of(ty), ground_cases_of(ty')) {
  | (Hole, Hole) => (result, state)
  | (Ground, Ground) =>
    /* if two types are ground and consistent, then they are eq */
    (result, state)
  | (Ground, Hole) =>
    /* can't remove the cast or do anything else here, so we're done */
    (BoxedValue(Cast(d1', ty, ty')), state)
  | (Hole, Ground) =>
    /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
    switch (d1') {
    | Cast(d1'', ty'', Hole) =>
      if (HTyp.eq(ty'', ty')) {
        (BoxedValue(d1''), state);
      } else {
        (Indet(FailedCast(d1', ty, ty')), state);
      }
    | _ =>
      // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
      (InvalidInput(CastBVHoleGround), state)
    }
  | (Hole, NotGroundOrHole(ty'_grounded)) =>
    /* ITExpand rule */
    let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
    evaluate(d', ~state);
  | (NotGroundOrHole(ty_grounded), Hole) =>
    /* ITGround rule */
    let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
    evaluate(d', ~state);
  | (Ground, NotGroundOrHole(_))
  | (NotGroundOrHole(_), Ground) =>
    /* can't do anything when casting between diseq, non-hole types */
    (BoxedValue(Cast(d1', ty, ty')), state)
  | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
    /* they might be eq in this case, so remove cast if so */
    if (HTyp.eq(ty, ty')) {
      (result, state);
    } else {
      (BoxedValue(Cast(d1', ty, ty')), state);
    }
  }
and eval_cast_indet = (ty, ty', result, state, indet_val) =>
  switch (ground_cases_of(ty), ground_cases_of(ty')) {
  | (Hole, Hole) => (result, state)
  | (Ground, Ground) =>
    /* if two types are ground and consistent, then they are eq */
    (result, state)
  | (Ground, Hole) =>
    /* can't remove the cast or do anything else here, so we're done */
    (Indet(Cast(indet_val, ty, ty')), state)
  | (Hole, Ground) =>
    switch (indet_val) {
    | Cast(d1'', ty'', Hole) =>
      if (HTyp.eq(ty'', ty')) {
        (Indet(d1''), state);
      } else {
        (Indet(FailedCast(indet_val, ty, ty')), state);
      }
    | _ => (Indet(Cast(indet_val, ty, ty')), state)
    }
  | (Hole, NotGroundOrHole(ty'_grounded)) =>
    /* ITExpand rule */
    let d' =
      DHExp.Cast(Cast(indet_val, ty, ty'_grounded), ty'_grounded, ty');
    evaluate(d', ~state);
  | (NotGroundOrHole(ty_grounded), Hole) =>
    /* ITGround rule */
    let d' = DHExp.Cast(Cast(indet_val, ty, ty_grounded), ty_grounded, ty');
    evaluate(d', ~state);
  | (Ground, NotGroundOrHole(_))
  | (NotGroundOrHole(_), Ground) =>
    /* can't do anything when casting between diseq, non-hole types */
    (Indet(Cast(indet_val, ty, ty')), state)
  | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
    /* it might be eq in this case, so remove cast if so */
    if (HTyp.eq(ty, ty')) {
      (result, state);
    } else {
      (Indet(Cast(indet_val, ty, ty')), state);
    }
  };
