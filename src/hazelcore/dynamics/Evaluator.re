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
type result = EvaluatorResult.t;

[@deriving sexp]
type state = EvalState.t;

[@deriving sexp]
type report = (result, state);

let unbox_result: result => DHExp.t =
  fun
  | Indet(d)
  | BoxedValue(d) => d;

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
  | (_, Fun(_, _, _)) => DoesNotMatch
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
  | (Cons(_) | ListLit(_), Cast(d, List(ty1), List(ty2))) =>
    matches_cast_Cons(dp, d, [(ty1, ty2)])
  | (Cons(_) | ListLit(_), Cast(d, Hole, List(_))) => matches(dp, d)
  | (Cons(_) | ListLit(_), Cast(d, List(_), Hole)) => matches(dp, d)
  | (Cons(_, _), Cons(_, _))
  | (ListLit(_, _), Cons(_, _))
  | (Cons(_, _), ListLit(_))
  | (ListLit(_), ListLit(_)) => matches_cast_Cons(dp, d, [])
  | (Cons(_) | ListLit(_), _) => DoesNotMatch
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
  | Fun(_, _, _) => DoesNotMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | TestLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | ListLit(_, _, _, _, _, _) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Pair(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_, _, _) => IndetMatch
  | NonEmptyHole(_, _, _, _, _) => IndetMatch
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
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | TestLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | Inj(_, _, _) => DoesNotMatch
  | ListLit(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_, _, _) => IndetMatch
  | NonEmptyHole(_, _, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  }
and matches_cast_Cons =
    (dp: DHPat.t, d: DHExp.t, elt_casts: list((HTyp.t, HTyp.t)))
    : match_result =>
  switch (d) {
  | ListLit(_, _, _, _, _, []) =>
    switch (dp) {
    | ListLit(_, []) => Matches(Environment.empty)
    | _ => DoesNotMatch
    }

  | ListLit(u, i, sigma, err, ty, [dhd, ...dtl] as ds) =>
    switch (dp) {
    | Cons(dp1, dp2) =>
      switch (matches(dp1, DHExp.apply_casts(dhd, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (HTyp.t, HTyp.t)) => {
              let (ty1, ty2) = c;
              (HTyp.List(ty1), HTyp.List(ty2));
            },
            elt_casts,
          );
        let d2 = DHExp.ListLit(u, i, sigma, err, ty, dtl);
        switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env2) => Matches(Environment.union(env1, env2))
        };
      }
    | ListLit(_, dps) =>
      switch (ListUtil.opt_zip(dps, ds)) {
      | None => DoesNotMatch
      | Some(lst) =>
        lst
        |> List.map(((dp, d)) =>
             matches(dp, DHExp.apply_casts(d, elt_casts))
           )
        |> List.fold_left(
             (match1, match2) =>
               switch (match1, match2) {
               | (DoesNotMatch, _)
               | (_, DoesNotMatch) => DoesNotMatch
               | (IndetMatch, _)
               | (_, IndetMatch) => IndetMatch
               | (Matches(env1), Matches(env2)) =>
                 Matches(Environment.union(env1, env2))
               },
             Matches(Environment.empty),
           )
      }
    | _ => failwith("called matches_cast_Cons with non-list pattern")
    }
  | Cons(d1, d2) =>
    switch (dp) {
    | Cons(dp1, dp2) =>
      switch (matches(dp1, DHExp.apply_casts(d1, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
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
    | ListLit(_, []) => DoesNotMatch
    | ListLit(ty, [dphd, ...dptl]) =>
      switch (matches(dphd, DHExp.apply_casts(d1, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (HTyp.t, HTyp.t)) => {
              let (ty1, ty2) = c;
              (HTyp.List(ty1), HTyp.List(ty2));
            },
            elt_casts,
          );
        let dp2 = DHPat.ListLit(ty, dptl);
        switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
        | DoesNotMatch => DoesNotMatch
        | IndetMatch => IndetMatch
        | Matches(env2) => Matches(Environment.union(env1, env2))
        };
      }
    | _ => failwith("called matches_cast_Cons with non-list pattern")
    }
  | Cast(d', List(ty1), List(ty2)) =>
    matches_cast_Cons(dp, d', [(ty1, ty2), ...elt_casts])
  | Cast(d', List(_), Hole) => matches_cast_Cons(dp, d', elt_casts)
  | Cast(d', Hole, List(_)) => matches_cast_Cons(dp, d', elt_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | TestLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | Inj(_, _, _) => DoesNotMatch
  | Pair(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_, _, _) => IndetMatch
  | NonEmptyHole(_, _, _, _, _) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
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
  | Fun(dp, ty, d3) =>
    if (DHPat.binds_var(x, dp)) {
      d2;
    } else {
      let d3 = subst_var(d1, x, d3);
      Fun(dp, ty, d3);
    }
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
  | Triv => d2
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Cons(d3, d4);
  | ListLit(u, i, sigma, err, t, types) =>
    let subst_sigma = subst_var_env(d1, x, sigma);
    let subst_types = List.map(subst_var(d1, x), types);
    ListLit(u, i, subst_sigma, err, t, subst_types);
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
  | TestLit(n) => TestLit(n)
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
    (~state: state, op: DHExp.BinBoolOp.t, b1: bool): option(report) =>
  switch (op, b1) {
  | (Or, true) => Some((BoxedValue(BoolLit(true)), state))
  | (And, false) => Some((BoxedValue(BoolLit(false)), state))
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

let rec evaluate = (~state: state=EvalState.init, d: DHExp.t): report => {
  let state = EvalState.take_step(state);
  switch (d) {
  | BoundVar(x) => raise(EvaluatorError.Exception(FreeInvalidVar(x)))
  | Let(dp, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(d1), state)
    | (Indet(d1), state) =>
      switch (matches(dp, d1)) {
      | IndetMatch => (Indet(d), state)
      | DoesNotMatch => (Indet(d), state)
      | Matches(env) => evaluate(subst(env, d2), ~state)
      }
    }
  | FixF(_) when state.fuel <= 0 => (
      Indet(InvalidOperation(d, OutOfFuel)),
      state,
    )
  | FixF(x, _, d1) => evaluate(subst_var(d, x, d1), ~state)
  | Fun(_, _, _) => (BoxedValue(d), state)
  | Ap(d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(Fun(dp, _, d3)), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2), state)
      | (Indet(d2), state) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch => (Indet(d), state)
        | IndetMatch => (Indet(d), state)
        | Matches(env) =>
          /* beta rule */
          evaluate(subst(env, d3), ~state)
        }
      }
    | (BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), state)
    | (Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2'), state)
      | (Indet(d2'), state) =>
        /* ap cast rule */
        evaluate(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'), ~state)
      }
    | (BoxedValue(d1'), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')))
    | (Indet(d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2'), state)
      | (Indet(d2'), state) => (Indet(Ap(d1', d2')), state)
      }
    }
  | ApBuiltin(ident, args) => evaluate_ap_builtin(ident, args, ~state)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv
  | TestLit(_) => (BoxedValue(d), state)
  | Sequence(d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(_), state) => evaluate(d2, ~state)
    | (Indet(d1), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2), state)
      | (Indet(d2), state) => (Indet(Sequence(d1, d2)), state)
      }
    }
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(BoolLit(b1) as d1'), state) =>
      switch (eval_bin_bool_op_short_circuit(op, b1, ~state)) {
      | Some((b3, state)) => (b3, state)
      | None =>
        switch (evaluate(d2, ~state)) {
        | (BoxedValue(BoolLit(b2)), state) => (
            BoxedValue(eval_bin_bool_op(op, b1, b2)),
            state,
          )
        | (BoxedValue(d2'), _) =>
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
        | (Indet(d2'), state) => (Indet(BinBoolOp(op, d1', d2')), state)
        }
      }
    | (BoxedValue(d1'), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
    | (Indet(d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2'), state)
      | (Indet(d2'), state) => (Indet(BinBoolOp(op, d1', d2')), state)
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(IntLit(n1) as d1'), state) =>
      switch (evaluate(d2, ~state)) {
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
      | (BoxedValue(d2'), _) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
      | (Indet(d2'), state) => (Indet(BinIntOp(op, d1', d2')), state)
      }
    | (BoxedValue(d1'), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
    | (Indet(d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2'), state)
      | (Indet(d2'), state) => (Indet(BinIntOp(op, d1', d2')), state)
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(FloatLit(f1) as d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(FloatLit(f2)), state) => (
          BoxedValue(eval_bin_float_op(op, f1, f2)),
          state,
        )
      | (BoxedValue(d2'), _) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
      | (Indet(d2'), state) => (Indet(BinFloatOp(op, d1', d2')), state)
      }
    | (BoxedValue(d1'), _) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
    | (Indet(d1'), state) =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2'), state)
      | (Indet(d2'), state) => (Indet(BinFloatOp(op, d1', d2')), state)
      }
    }
  | Inj(ty, side, d1) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(d1'), state) => (BoxedValue(Inj(ty, side, d1')), state)
    | (Indet(d1'), state) => (Indet(Inj(ty, side, d1')), state)
    }
  | Pair(d1, d2) =>
    let (d1, state) = evaluate(d1, ~state);
    let (d2, state) = evaluate(d2, ~state);
    switch (d1, d2) {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => (Indet(Pair(d1, d2)), state)
    | (BoxedValue(d1), BoxedValue(d2)) => (BoxedValue(Pair(d1, d2)), state)
    };
  | Cons(d1, d2) =>
    let (d1, state) = evaluate(d1, ~state);
    let (d2, state) = evaluate(d2, ~state);
    switch (d1, d2) {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => (Indet(Cons(d1, d2)), state)
    | (BoxedValue(d1), BoxedValue(d2)) =>
      switch (d2) {
      | ListLit(x1, x2, x3, x4, x5, lst) => (
          BoxedValue(ListLit(x1, x2, x3, x4, x5, [d1, ...lst])),
          state,
        )
      | Cast(ListLit(x1, x2, x3, x4, x5, lst), List(ty), List(ty')) => (
          BoxedValue(
            Cast(
              ListLit(x1, x2, x3, x4, x5, [d1, ...lst]),
              List(ty),
              List(ty'),
            ),
          ),
          state,
        )
      | _ => raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)))
      }
    };
  | ListLit(x1, x2, x3, x4, ty, lst) =>
    let (evaluated_lst, state) =
      List.fold_right(
        (ele, (ele_list, state)) => {
          switch (evaluate(ele, ~state)) {
          | (BoxedValue(ele'), state) => ([ele'] @ ele_list, state)
          | _ => ([ele, ...ele_list], state)
          }
        },
        lst,
        ([], state),
      );
    (BoxedValue(ListLit(x1, x2, x3, x4, ty, evaluated_lst)), state);
  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case(None, d1, rules, n, ~state)
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
    evaluate_case(Some((u, i, sigma)), d1, rules, n, ~state)
  | EmptyHole(_) => (Indet(d), state)
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(d1'), state)
    | (Indet(d1'), state) => (
        Indet(NonEmptyHole(reason, u, i, sigma, d1')),
        state,
      )
    }
  | FreeVar(_) => (Indet(d), state)
  | ExpandingKeyword(_) => (Indet(d), state)
  | InvalidText(_) => (Indet(d), state)
  | Cast(d1, ty, ty') =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(d1') as result, state) =>
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
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')))
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
    | (Indet(d1') as result, state) =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => (result, state)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        (result, state)
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        (Indet(Cast(d1', ty, ty')), state)
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            (Indet(d1''), state);
          } else {
            (Indet(FailedCast(d1', ty, ty')), state);
          }
        | _ => (Indet(Cast(d1', ty, ty')), state)
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
        (Indet(Cast(d1', ty, ty')), state)
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          (result, state);
        } else {
          (Indet(Cast(d1', ty, ty')), state);
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(d1'), state)
    | (Indet(d1'), state) => (Indet(FailedCast(d1', ty, ty')), state)
    }
  | InvalidOperation(d, err) => (Indet(InvalidOperation(d, err)), state)
  };
}
and evaluate_case =
    (
      ~state: state,
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : report =>
  switch (evaluate(scrut, ~state)) {
  | (BoxedValue(scrut), state)
  | (Indet(scrut), state) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      switch (inconsistent_info) {
      | None => (Indet(ConsistentCase(case)), state)
      | Some((u, i, sigma)) => (
          Indet(InconsistentBranches(u, i, sigma, case)),
          state,
        )
      };
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | IndetMatch =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => (Indet(ConsistentCase(case)), state)
        | Some((u, i, sigma)) => (
            Indet(InconsistentBranches(u, i, sigma, case)),
            state,
          )
        };
      | Matches(env) => evaluate(subst(env, d), ~state)
      | DoesNotMatch =>
        evaluate_case(
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
          ~state,
        )
      }
    }
  }
/* Evaluate the application of a built-in function. */
and evaluate_ap_builtin =
    (~state: state, ident: string, args: list(DHExp.t)): report => {
  switch (Builtins.lookup_form(ident)) {
  | Some((eval, _)) => (eval(args, d => fst(evaluate(d, ~state))), state)
  | None => raise(EvaluatorError.Exception(InvalidBuiltin(ident)))
  };
};
