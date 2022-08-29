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

[@deriving sexp]
type eval_input = (DHExp.t, state);

let unbox_result: result => DHExp.t =
  fun
  | Indet(d)
  | BoxedValue(d) => d;

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
  | (BoolLit(_), Cast(d, dty1, dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Hole, Bool)
    | (Bool, Hole) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
  | (BoolLit(_), _) => DoesNotMatch
  | (IntLit(n1), IntLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (IntLit(_), Cast(d, dty1, dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Hole, Int)
    | (Int, Hole) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
  | (IntLit(_), _) => DoesNotMatch
  | (FloatLit(n1), FloatLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (FloatLit(_), Cast(d, dty1, dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Hole, Float)
    | (Float, Hole) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
  | (FloatLit(_), _) => DoesNotMatch
  | (Inj(side1, dp), Inj(_, side2, d)) =>
    switch (side1, side2) {
    | (L, L)
    | (R, R) => matches(dp, d)
    | _ => DoesNotMatch
    }
  | (Inj(side, dp), Cast(d, (tyvars1, _) as dty1, (tyvars2, _) as dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Sum(tyL1, tyR1), Sum(tyL2, tyR2)) =>
      let dtys: (DHTyp.t, DHTyp.t, DHTyp.t, DHTyp.t) = (
        (tyvars1, tyL1),
        (tyvars1, tyR1),
        (tyvars2, tyL2),
        (tyvars2, tyR2),
      );
      matches_cast_Inj(side, dp, d, [dtys]);
    | (Sum(_), _)
    | (_, Sum(_)) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
  | (Inj(_), _) => DoesNotMatch
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
  | (Pair(dp1, dp2), Cast(d, (tyvars1, _) as dty1, (tyvars2, _) as dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Prod([head1, ...tail1]), Prod([head2, ...tail2])) =>
      matches_cast_Pair(
        dp1,
        dp2,
        d,
        [((tyvars1, head1), (tyvars2, head2))],
        List.combine(
          DHTyp.many(tyvars1, tail1),
          DHTyp.many(tyvars2, tail2),
        ),
      )
    | (Prod(_), _)
    | (_, Prod(_)) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
  | (Pair(_), _) => DoesNotMatch
  | (Triv, Triv) => Matches(Environment.empty)
  | (Triv, Cast(d, dty1, dty2))
      when
        DHTyp.head_normalize(dty1) == Prod([])
        || DHTyp.head_normalize(dty2) == Prod([]) =>
    matches(dp, d)
  | (Triv, _) => DoesNotMatch
  | (ListNil, Cast(d, dty1, dty2))
      when
        DHTyp.head_normalize(dty1) == Prod([])
        || DHTyp.head_normalize(dty2) == Prod([]) =>
    matches(dp, d)
  | (ListNil, ListNil(_)) => Matches(Environment.empty)
  | (ListNil, Cast(d, dty1, dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Hole, List(_))
    | (List(_), Hole)
    | (List(_), List(_)) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
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
  | (Cons(dp1, dp2), Cast(d, (tyvars1, _) as dty1, (tyvars2, _) as dty2)) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (List(ty1), List(ty2)) =>
      matches_cast_Cons(dp1, dp2, d, [((tyvars1, ty1), (tyvars2, ty2))])
    | (Hole, List(_))
    | (List(_), Hole) => matches(dp, d)
    | (_, _) => DoesNotMatch
    }
  | (Cons(_, _), _) => DoesNotMatch
  | (Ap(_, _), _) => DoesNotMatch
  }
and matches_cast_Inj =
    (
      side: InjSide.t,
      dp: DHPat.t,
      d: DHExp.t,
      casts: list((DHTyp.t, DHTyp.t, DHTyp.t, DHTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Inj(_, side', d') =>
    switch (side, side') {
    | (L, L)
    | (R, R) =>
      let side_casts =
        List.map(
          (c: (DHTyp.t, DHTyp.t, DHTyp.t, DHTyp.t)) => {
            let (dtyL1, dtyR1, dtyL2, dtyR2) = c;
            switch (side) {
            | L => (dtyL1, dtyL2)
            | R => (dtyR1, dtyR2)
            };
          },
          casts,
        );
      matches(dp, DHExp.apply_casts(d', side_casts));
    | _ => DoesNotMatch
    }
  | Cast(d', (tyvars1, _) as dty1, (tyvars2, _) as dty2) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Sum(tyL1, tyR1), Sum(tyL2, tyR2)) =>
      let dtys = (
        (tyvars1, tyL1),
        (tyvars1, tyR1),
        (tyvars2, tyL2),
        (tyvars2, tyR2),
      );
      matches_cast_Inj(side, dp, d', [dtys, ...casts]);
    | (Sum(_), Hole)
    | (Hole, Sum(_)) => matches_cast_Inj(side, dp, d', casts)
    | (_, _) => DoesNotMatch
    }
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | TyAlias(_) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | TypFun(_, _) => DoesNotMatch
  | TypApp(_, _) => IndetMatch
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_) => DoesNotMatch
  | TestLit(_) => DoesNotMatch
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
  }
and matches_cast_Pair =
    (
      dp1: DHPat.t,
      dp2: DHPat.t,
      d: DHExp.t,
      left_casts: list((DHTyp.t, DHTyp.t)),
      right_casts: list((DHTyp.t, DHTyp.t)),
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
  | Cast(d', (tyvars1, _) as dty1, (tyvars2, _) as dty2) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (Prod([]), Prod([])) =>
      matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
    | (Prod([head1, ...tail1]), Prod([head2, ...tail2])) =>
      matches_cast_Pair(
        dp1,
        dp2,
        d',
        [((tyvars1, head1), (tyvars2, head2)), ...left_casts],
        List.combine(DHTyp.many(tyvars1, tail1), DHTyp.many(tyvars1, tail2))
        @ right_casts,
      )
    | (Prod(_), Hole)
    | (Hole, Prod(_)) =>
      matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
    | (_, _) => DoesNotMatch
    }
  | TyAlias(_) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
  | ApBuiltin(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_) => DoesNotMatch
  | TestLit(_) => DoesNotMatch
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
  }
and matches_cast_Cons =
    (
      dp1: DHPat.t,
      dp2: DHPat.t,
      d: DHExp.t,
      elt_casts: list((DHTyp.t, DHTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Cons(d1, d2) =>
    switch (matches(dp1, DHExp.apply_casts(d1, elt_casts))) {
    | DoesNotMatch => DoesNotMatch
    | IndetMatch =>
      let list_casts =
        List.map(
          (c: (DHTyp.t, DHTyp.t)) => {
            let (dty1, dty2) = c;
            (DHTyp.list(dty1), DHTyp.list(dty2));
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
          (c: (DHTyp.t, DHTyp.t)) => {
            let (dty1, dty2) = c;
            (DHTyp.list(dty1), DHTyp.list(dty2));
          },
          elt_casts,
        );
      switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env2) => Matches(Environment.union(env1, env2))
      };
    }
  | Cast(d', (tyvars1, _) as dty1, (tyvars2, _) as dty2) =>
    switch (DHTyp.head_normalize(dty1), DHTyp.head_normalize(dty2)) {
    | (List(ty1), List(ty2)) =>
      matches_cast_Cons(
        dp1,
        dp2,
        d',
        [((tyvars1, ty1), (tyvars2, ty2)), ...elt_casts],
      )
    | (List(_), Hole) => matches_cast_Cons(dp1, dp2, d', elt_casts)
    | (Hole, List(_)) => matches_cast_Cons(dp1, dp2, d', elt_casts)
    | (_, _) => DoesNotMatch
    }
  | TyAlias(_) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_, _, _, _) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
  | ApBuiltin(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_) => DoesNotMatch
  | TestLit(_) => DoesNotMatch
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
  | TyAlias(dp, dty, d3) =>
    let d3 = subst_var(d1, x, d3);
    TyAlias(dp, dty, d3);
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
  | TypFun(dp, d3) =>
    let d3 = subst_var(d1, x, d3);
    TypFun(dp, d3);
  | TypApp(d3, dty) =>
    let d3 = subst_var(d1, x, d3);
    TypApp(d3, dty);
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
  | Fun(_)
  | TestLit(_) => (BoxedValue(d), state)
  | FreeVar(_)
  | ExpandingKeyword(_)
  | InvalidText(_)
  | EmptyHole(_) => (Indet(d), state)
  | BoundVar(x) => raise(EvaluatorError.Exception(FreeInvalidVar(x)))
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
  | TyAlias(_, _, d) => evaluate(d)
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

  | ApBuiltin(ident, args) =>
    switch (Builtins.lookup_form(ident)) {
    | Some((eval, _)) => (eval(args, d => fst(evaluate(d, ~state))), state)
    | None => raise(EvaluatorError.Exception(InvalidBuiltin(ident)))
    }

  | Ap(d1, d2) =>
    let (result, state) = evaluate(d1, ~state);
    switch (result) {
    | BoxedValue(TestLit(n)) => eval_test(n, d2, state)
    | BoxedValue(Fun(dp, _, d3)) =>
      eval_bind((d2, state), ((d2', state)) =>
        switch (matches(dp, d2')) {
        | DoesNotMatch
        | IndetMatch => (Indet(d), state)
        | Matches(env) =>
          /* beta rule */
          evaluate(subst(env, d3), ~state)
        }
      )
    | BoxedValue(Cast(d1', (ctx, _) as dty, (ctx', _) as dty'))
    | Indet(Cast(d1', (ctx, _) as dty, (ctx', _) as dty')) =>
      switch (DHTyp.head_normalize(dty), DHTyp.head_normalize(dty')) {
      | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
        eval_bind((d2, state), ((d2', state)) =>
          evaluate(
            Cast(
              Ap(d1', Cast(d2', (ctx', ty1'), (ctx, ty1))),
              (ctx, ty2),
              (ctx', ty2'),
            ),
            ~state,
          )
        )
      | _ =>
        switch (result) {
        | BoxedValue(d1') =>
          raise(EvaluatorError.Exception(InvalidBoxedFun(d1')))
        | Indet(d1') =>
          switch (evaluate(d2, ~state)) {
          | (BoxedValue(d2'), state)
          | (Indet(d2'), state) => (Indet(Ap(d1', d2')), state)
          }
        }
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')))
    | Indet(d1') =>
      switch (evaluate(d2, ~state)) {
      | (BoxedValue(d2'), state)
      | (Indet(d2'), state) => (Indet(Ap(d1', d2')), state)
      }
    };
  | ApBuiltin(ident, args) => evaluate_ap_builtin(ident, args)
  | TypFun(_, _) => BoxedValue(d)
  | TypApp(d, _) => evaluate(d)
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => BoxedValue(d)
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(BoolLit(b1) as d1'), state) =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => (b3, state)
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
      eval_bind_indet((d2, state), d2' => BinBoolOp(op, d1', d2'))
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
      eval_bind_indet((d2, state), d2' => BinIntOp(op, d1', d2'))
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
      eval_bind_indet((d2, state), d2' => BinFloatOp(op, d1', d2'))
    }
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let mk_non_empty = d1' => DHExp.NonEmptyHole(reason, u, i, sigma, d1');
    eval_bind_indet((d1, state), mk_non_empty);
  | FailedCast(d1, ty, ty') =>
    eval_bind_indet((d1, state), d1' => FailedCast(d1', ty, ty'))
  | InvalidOperation(d, err) =>
    eval_bind_indet((d, state), d' => InvalidOperation(d', err))
  | ConsistentCase(Case(scrut, rules, index) as case) =>
    let indet_res = EvaluatorResult.Indet(ConsistentCase(case));
    eval_case(indet_res, scrut, rules, index, state);
  | InconsistentBranches(u, i, sigma, Case(scrut, rules, index) as case) =>
    let indet_res =
      EvaluatorResult.Indet(InconsistentBranches(u, i, sigma, case));
    eval_case(indet_res, scrut, rules, index, state);

  | Cast(d1, ty, ty') =>
    switch (evaluate(d1, ~state)) {
    | (BoxedValue(boxed_val), state) =>
      eval_cast(
        d => EvaluatorResult.BoxedValue(d),
        ty,
        ty',
        state,
        boxed_val,
      )
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
and eval_bind_indet = ((d, state): eval_input, f: DHExp.t => DHExp.t): report =>
  bind(evaluate(d, ~state), ((d', state)) => (Indet(f(d')), state))
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
  | None => (indet_res, state) // TODO(andrew): should this be an InvalidOperation?
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
      ty: DHTyp.t,
      ty': DHTyp.t,
      state: state,
      value: DHExp.t,
    )
    : report => {
  switch (DHTyp.ground_cases_of(ty), DHTyp.ground_cases_of(ty')) {
  | (Hole, Hole)
  | (Ground, Ground) =>
    /* if two types are ground and consistent, then they are eq */
    (cons(value), state)
  | (Ground, Hole) =>
    /* can't remove the cast or do anything else here, so we're done */
    (cons(Cast(value, ty, ty')), state)
  | (NotGroundOrHole(_), NotGroundOrHole(_)) when DHTyp.equivalent(ty, ty') => (
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
    | Cast(d1'', ty'', (_, ty'''))
        when HTyp.is_hole(ty''') && DHTyp.equivalent(ty'', ty') => (
        cons(d1''),
        state,
      )
    | Cast(_, _, (_, ty'')) when HTyp.is_hole(ty'') => (
        Indet(FailedCast(value, ty, ty')),
        state,
      )
    | d =>
      switch (cons(Triv)) {
      | BoxedValue(_) =>
        raise(EvaluatorError.Exception(CastBVHoleGround(d)))
      | Indet(_) => (cons(Cast(value, ty, ty')), state)
      }
    }
  | (Hole, NotGroundOrHole(ty_grounded)) =>
    /* ITExpand rule */
    let d' =
      DHExp.Cast(
        Cast(
          value,
          ty,
          (InitialContext.ctx, HTyp.of_normalized(ty_grounded)),
        ),
        (InitialContext.ctx, HTyp.of_normalized(ty_grounded)),
        ty',
      );
    evaluate(d', ~state);
  | (NotGroundOrHole(ty_grounded), Hole) =>
    /* ITGround rule */
    let d' =
      DHExp.Cast(
        Cast(
          value,
          ty,
          (InitialContext.ctx, HTyp.of_normalized(ty_grounded)),
        ),
        (InitialContext.ctx, HTyp.of_normalized(ty_grounded)),
        ty',
      );
    evaluate(d', ~state);
  };
}
and eval_test_eq =
    (
      bin_op_fn: (DHExp.t, DHExp.t) => DHExp.t,
      d1: DHExp.t,
      d2: DHExp.t,
      state: state,
    )
    : (DHExp.t, report) => {
  let (d1, state) = evaluate(d1, ~state);
  let (d2, state) = evaluate(d2, ~state);
  let d = bin_op_fn(unbox_result(d1), unbox_result(d2));
  (d, evaluate(d, ~state));
}
and eval_test = (n: int, d: DHExp.t, state: state): report => {
  /* For binary operators and unary/binary applications,
     display the evaluated arguments */
  let (show_d, (res_d, state)) =
    switch (DHExp.strip_casts(d)) {
    | BinBoolOp(op, d1, d2) =>
      let mk_op = (d1, d2) => DHExp.BinBoolOp(op, d1, d2);
      eval_test_eq(mk_op, d1, d2, state);
    | BinIntOp(op, d1, d2) =>
      let mk_op = (d1, d2) => DHExp.BinIntOp(op, d1, d2);
      eval_test_eq(mk_op, d1, d2, state);
    | BinFloatOp(op, d1, d2) =>
      let mk_op = (d1, d2) => DHExp.BinFloatOp(op, d1, d2);
      eval_test_eq(mk_op, d1, d2, state);
    | Ap(Ap(d1, d2), d3) =>
      let (d1, state) = evaluate(d1, ~state);
      let (d2, state) = evaluate(d2, ~state);
      let (d3, state) = evaluate(d3, ~state);
      let d =
        DHExp.Ap(
          Ap(unbox_result(d1), unbox_result(d2)),
          unbox_result(d3),
        );
      (d, evaluate(d, ~state));
    | Ap(d1, d2) =>
      let mk = (d1, d2) => DHExp.Ap(d1, d2);
      eval_test_eq(mk, d1, d2, state);
    | _ =>
      let (d, state) = evaluate(d, ~state);
      (unbox_result(d), (d, state));
    };
  let test_status: TestStatus.t =
    switch (res_d) {
    | BoxedValue(BoolLit(true)) => Pass
    | BoxedValue(BoolLit(false)) => Fail
    | _ => Indet
    };
  let state = EvalState.add_test(state, n, (show_d, test_status));
  let result =
    switch (res_d) {
    | BoxedValue(BoolLit(_)) => EvaluatorResult.BoxedValue(Triv)
    | _ => Indet(Ap(TestLit(n), d))
    };
  (result, state);
};
