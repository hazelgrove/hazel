[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

[@deriving sexp]
type result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type state = EvalState.t;

[@deriving sexp]
type report = (result, state);

[@deriving sexp]
type eval_input = (DHExp.t, state);

let unbox_result: result => DHExp.t =
  fun
  | Indet(d)
  | BoxedValue(d) => d;

let report_assert = (res_d: result, d: DHExp.t, state, n): report => {
  let d_res = unbox_result(res_d);
  let eval_res =
    switch (d_res) {
    | BoolLit(_) => BoxedValue(Triv)
    | _ => Indet(Ap(AssertLit(n), d))
    };
  let assert_status: AssertStatus.t =
    switch (d_res) {
    | BoolLit(true) => Pass
    | BoolLit(false) => Fail
    | _ => Indet
    };
  let state = EvalState.add_assert(state, n, (d, assert_status));
  (eval_res, state);
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
  | (_, EmptyHole(_, _, _)) => IndetMatch
  | (_, NonEmptyHole(_, _, _, _, _)) => IndetMatch
  | (_, FailedCast(_, _, _)) => IndetMatch
  | (_, InvalidOperation(_)) => IndetMatch
  | (_, FreeVar(_, _, _, _)) => IndetMatch
  | (_, InvalidText(_)) => IndetMatch
  | (_, Let(_, _, _)) => IndetMatch
  | (_, FixF(_, _, _)) => DoesNotMatch
  | (_, Lam(_, _, _)) => DoesNotMatch
  | (_, Ap(_, _)) => IndetMatch
  | (_, BinBoolOp(_, _, _)) => IndetMatch
  | (_, BinIntOp(_, _, _)) => IndetMatch
  | (_, BinFloatOp(_, _, _)) => IndetMatch
  | (_, ConsistentCase(Case(_, _, _))) => IndetMatch
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
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
  | Ap(_, _) => IndetMatch
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
  | EmptyHole(_, _, _) => IndetMatch
  | NonEmptyHole(_, _, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  | Sequence(_)
  | AssertLit(_)
  | SameLit(_) => DoesNotMatch
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
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
  | Ap(_, _) => IndetMatch
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
  | EmptyHole(_, _, _) => IndetMatch
  | NonEmptyHole(_, _, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  | Sequence(_)
  | AssertLit(_)
  | SameLit(_) => DoesNotMatch
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
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
  | Ap(_, _) => IndetMatch
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
  | EmptyHole(_, _, _) => IndetMatch
  | NonEmptyHole(_, _, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  | Sequence(_)
  | AssertLit(_)
  | SameLit(_) => DoesNotMatch
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
  | SameLit(n) => SameLit(n)
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

let bind' =
    (x: report, ~boxed: eval_input => report, ~indet: eval_input => report)
    : report =>
  switch (x) {
  | (BoxedValue(d'), state) => boxed((d', state))
  | (Indet(d'), state) => indet((d', state))
  };
let bind = (x: report, f: eval_input => report): report =>
  bind'(x, ~boxed=f, ~indet=f);

let rec evaluate = (~state: state=EvalState.init, d: DHExp.t): report => {
  let state = EvalState.take_step(state);
  switch (d) {
  | BoolLit(_)
  | ListNil(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv
  | Lam(_)
  | AssertLit(_)
  | SameLit(_) => (BoxedValue(d), state)
  | FreeVar(_)
  | ExpandingKeyword(_)
  | InvalidText(_)
  | EmptyHole(_) => (Indet(d), state)
  | BoundVar(s) => raise(EvaluatorError.Exception(FreeInvalidVar(s)))
  | Inj(ty, side, d1) =>
    let mk_inj = d1' => DHExp.Inj(ty, side, d1');
    eval_unary_constructor((d1, state), mk_inj);
  | Pair(d1, d2) =>
    let mk_pair = (d1', d2') => DHExp.Pair(d1', d2');
    eval_binary_constructor((d1, state), d2, mk_pair);
  | Cons(d1, d2) =>
    let mk_cons = (d1', d2') => DHExp.Cons(d1', d2');
    eval_binary_constructor((d1, state), d2, mk_cons);
  | FixF(_) when state.fuel <= 0 => (
      Indet(InvalidOperation(d, OutOfFuel)),
      state,
    )
  | FixF(x, _, d1) =>
    let state = EvalState.burn_fuel(state);
    evaluate(subst_var(d, x, d1), ~state);
  | Let(dp, d1, d2) =>
    eval_bind((d1, state), ((d1', state)) =>
      switch (matches(dp, d1')) {
      | Matches(env) => evaluate(subst(env, d2), ~state)
      | IndetMatch
      | DoesNotMatch => (Indet(d), state)
      }
    )
  | Sequence(d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(_), state) => evaluate(d2, ~state)
    | (Indet(d1), state) =>
      eval_bind_indet((d2, state), d2 => DHExp.Sequence(d1, d2))
    }
  | Ap(d1, d2) =>
    switch (DHExp.strip_casts_value(d1)) {
    | Ap(SameLit(n), d1) => eval_same(n, d1, d2, state)
    | _ =>
      switch (evaluate(d1, ~state)) {
      | (BoxedValue(SameLit(_) as sl), _) =>
        //TODO(andrew): figure out if there's a nicer way to do this
        evaluate(
          Lam(Var("@"), Hole, Ap(Ap(sl, d2), BoundVar("@"))),
          ~state,
        )
      | (BoxedValue(AssertLit(n)), state) => eval_assert(n, d2, state)
      | (BoxedValue(Lam(dp, _, d3)), state) =>
        eval_bind((d2, state), ((d2', state)) =>
          switch (matches(dp, d2')) {
          | DoesNotMatch
          | IndetMatch => (Indet(d), state)
          | Matches(env) =>
            /* beta rule */
            evaluate(subst(env, d3), ~state)
          }
        )
      | (BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), state)
      | (Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), state) =>
        eval_bind((d2, state), ((d2', state)) =>
          evaluate(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'), ~state)
        )
      | (BoxedValue(d), _) =>
        raise(EvaluatorError.Exception(InvalidBoxedLam(d)))
      | (Indet(d1'), state) =>
        eval_bind_indet((d2, state), d2' => Ap(d1', d2'))
      }
    }

  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(BoolLit(b1) as d1'), state) =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b) => (b, state)
      | None =>
        switch (evaluate(d2, ~state)) {
        | (BoxedValue(BoolLit(b2)), state) => (
            BoxedValue(eval_bin_bool_op(op, b1, b2)),
            state,
          )
        | (BoxedValue(d), _) =>
          raise(EvaluatorError.Exception(InvalidBoxedIntLit(d)))
        | (Indet(d2'), state) => (Indet(BinBoolOp(op, d1', d2')), state)
        }
      }
    | (BoxedValue(d), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d)))
    | (Indet(d1'), state) =>
      eval_bind_indet((d2, state), d2' => BinBoolOp(op, d1', d2'))
    }
  | BinIntOp(Equals, d1, d2) => eval_poly_eq(d1, d2, state)
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (Indet(d1'), state) =>
      eval_bind_indet((d2, state), d2' => BinIntOp(op, d1', d2'))
    | (BoxedValue(IntLit(n1) as d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (Indet(d2'), state) => (Indet(BinIntOp(op, d1', d2')), state)
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
      | (BoxedValue(d), _) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d)))
      }
    | (BoxedValue(d), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d)))
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (Indet(d1'), state) =>
      eval_bind_indet((d2, state), d2 => BinFloatOp(op, d1', d2))
    | (BoxedValue(FloatLit(f1) as d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (Indet(d2'), state) => (Indet(BinFloatOp(op, d1', d2')), state)
      | (BoxedValue(FloatLit(f2)), state) => (
          BoxedValue(eval_bin_float_op(op, f1, f2)),
          state,
        )
      | (BoxedValue(d), _) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d)))
      }
    | (BoxedValue(d), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d)))
    }
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let mk_non_empty = d1' => DHExp.NonEmptyHole(reason, u, i, sigma, d1');
    eval_bind_indet((d1, state), mk_non_empty);
  | FailedCast(d1, ty, ty') =>
    eval_bind_indet((d1, state), d1' => FailedCast(d1', ty, ty'))
  | InvalidOperation(d, err) =>
    eval_bind_indet((d, state), d' => InvalidOperation(d', err))
  | ConsistentCase(Case(scrut, rules, index) as case) =>
    let indet_res = Indet(ConsistentCase(case));
    eval_case(indet_res, scrut, rules, index, state);
  | InconsistentBranches(u, i, sigma, Case(scrut, rules, index) as case) =>
    let indet_res = Indet(InconsistentBranches(u, i, sigma, case));
    eval_case(indet_res, scrut, rules, index, state);
  | Cast(d1, ty, ty') =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(boxed_val), state) =>
      eval_cast(d => BoxedValue(d), ty, ty', state, boxed_val)
    | (Indet(indet_val), state) =>
      eval_cast(d => Indet(d), ty, ty', state, indet_val)
    }
  };
}
and eval_unary_constructor = ((d1, state): eval_input, cons) =>
  switch (evaluate(d1, ~state)) {
  | (Indet(d1'), state) => (Indet(cons(d1')), state)
  | (BoxedValue(d1'), state) => (BoxedValue(cons(d1')), state)
  }
and eval_binary_constructor = ((d1, state): eval_input, d2, cons) =>
  switch (evaluate(d1, ~state)) {
  | (Indet(d1'), state) =>
    eval_bind_indet((d2, state), d2' => cons(d1', d2'))
  | (BoxedValue(d1'), state) =>
    eval_unary_constructor((d2, state), d2' => cons(d1', d2'))
  }
and eval_bind = ((d, state): eval_input, f: eval_input => report): report =>
  bind(evaluate(d, ~state), f)
and eval_bind_nofx = (d_s: eval_input, f: DHExp.t => result): report =>
  eval_bind(d_s, ((d', state)) => (f(d'), state))
and eval_bind_indet = (d_s: eval_input, f: DHExp.t => DHExp.t): report =>
  eval_bind_nofx(d_s, d' => Indet(f(d')))
and eval_case =
    (
      indet_res: result,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      state: state,
    )
    : report => {
  let (res', state) = evaluate(scrut, ~state);
  let scrut = unbox_result(res');
  switch (List.nth_opt(rules, current_rule_index)) {
  | None => (indet_res, state) // TODO(andrew):should this be an InvalidOperation?
  | Some(Rule(dp, d)) =>
    switch (matches(dp, scrut)) {
    | IndetMatch => (indet_res, state)
    | Matches(env) => evaluate(subst(env, d), ~state)
    | DoesNotMatch =>
      eval_case(indet_res, scrut, rules, current_rule_index + 1, state)
    }
  };
}
and eval_cast =
    (
      cons: DHExp.t => result,
      ty: HTyp.t,
      ty': HTyp.t,
      state: state,
      value: DHExp.t,
    )
    : report => {
  switch (ground_cases_of(ty), ground_cases_of(ty')) {
  | (Hole, Hole)
  | (Ground, Ground) =>
    /* if two types are ground and consistent, then they are eq */
    (cons(value), state)
  | (Ground, Hole) =>
    /* can't remove the cast or do anything else here, so we're done */
    (cons(Cast(value, ty, ty')), state)
  | (NotGroundOrHole(_), NotGroundOrHole(_)) when HTyp.eq(ty, ty') => (
      cons(value),
      state,
    )
  | (Ground, NotGroundOrHole(_))
  | (NotGroundOrHole(_), Ground)
  | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
    /* can't do anything when casting between diseq, non-hole types */
    (cons(Cast(value, ty, ty')), state)
  | (Hole, Ground) =>
    switch (value) {
    | Cast(d1'', ty'', Hole) when HTyp.eq(ty'', ty') => (cons(d1''), state)
    | Cast(_, _, Hole) => (Indet(FailedCast(value, ty, ty')), state)
    | d =>
      switch (cons(Triv)) {
      | BoxedValue(_) =>
        raise(EvaluatorError.Exception(CastBVHoleGround(d)))
      | Indet(_) => (cons(Cast(value, ty, ty')), state)
      }
    }
  | (Hole, NotGroundOrHole(ty_grounded)) =>
    /* ITExpand rule */
    let d' = DHExp.Cast(Cast(value, ty, ty_grounded), ty_grounded, ty');
    evaluate(d', ~state);
  | (NotGroundOrHole(ty_grounded), Hole) =>
    /* ITGround rule */
    let d' = DHExp.Cast(Cast(value, ty, ty_grounded), ty_grounded, ty');
    evaluate(d', ~state);
  };
}
and eval_assert_eq =
    (
      bin_op_fn: (DHExp.t, DHExp.t) => DHExp.t,
      d1: DHExp.t,
      d2: DHExp.t,
      state: state,
    )
    : report => {
  let (d1, state) = evaluate(d1, ~state);
  let (d2, state) = evaluate(d2, ~state);
  let d = bin_op_fn(unbox_result(d1), unbox_result(d2));
  evaluate(d, ~state);
}
and eval_assert = (n: int, d: DHExp.t, state: state): report => {
  let (res_d, state) =
    switch (d) {
    | BinIntOp((Equals | LessThan | GreaterThan) as op, d1, d2) =>
      let mk_op = (d1, d2) => DHExp.BinIntOp(op, d1, d2);
      eval_assert_eq(mk_op, d1, d2, state);
    | BinFloatOp((FEquals | FLessThan | FGreaterThan) as op, d1, d2) =>
      let mk_op = (d1, d2) => DHExp.BinFloatOp(op, d1, d2);
      eval_assert_eq(mk_op, d1, d2, state);
    | _ => evaluate(d, ~state)
    };
  report_assert(res_d, d, state, n);
}
and eval_same = (n: int, d1: DHExp.t, d2: DHExp.t, state: state): report => {
  let (d1, state) = evaluate(d1, ~state);
  let (d2, state) = evaluate(d2, ~state);
  let d1_clean = DHExp.strip_casts_value(unbox_result(d1));
  let d2_clean = DHExp.strip_casts_value(unbox_result(d2));
  let d: DHExp.t = Ap(Ap(SameLit(n), d1_clean), d2_clean);
  let is_type_error =
    switch (d2_clean) {
    | NonEmptyHole(_) => true
    | _ => false
    };
  let assert_status: AssertStatus.t =
    switch (d1, d2) {
    | _ when is_type_error => Fail // types are wrong
    | (Indet(_), _)
    | (_, Indet(_)) => Indet
    | (BoxedValue(_), BoxedValue(_)) =>
      switch (DHExp.dhexp_diff_value(d1_clean, d2_clean)) {
      | ([], _) => Pass
      | _ => Fail
      }
    };
  let eval_res = BoxedValue(DHExp.Triv); //BoxedValue(d);
  let state = EvalState.add_assert(state, n, (d, assert_status));
  (eval_res, state);
}
and eval_poly_eq = (d1: DHExp.t, d2: DHExp.t, state: state): report =>
  //TODO(andrew):cleanup
  //do i want this to still evaluate if it is ill-typed?
  switch (evaluate(d1, ~state)) {
  | (Indet(d1'), state) =>
    eval_bind_indet((d2, state), d2' => BinIntOp(Equals, d1', d2'))
  | (BoxedValue(d1'), state) =>
    switch (evaluate(d2, ~state)) {
    | (Indet(d2'), state) => (Indet(BinIntOp(Equals, d1', d2')), state)
    | (BoxedValue(d2'), state) =>
      let d1_clean = DHExp.strip_casts_value(d1');
      let d2_clean = DHExp.strip_casts_value(d2');
      let res =
        switch (DHExp.dhexp_diff_value(d1_clean, d2_clean)) {
        | ([], _) => true
        | _ => false
        };
      (BoxedValue(BoolLit(res)), state);
    }
  };
