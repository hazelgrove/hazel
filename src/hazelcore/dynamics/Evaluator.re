[@deriving sexp]
type result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

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
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | TyAlias(_) => Indet
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
    | Indet =>
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
      | Indet
      | Matches(_) => Indet
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
      | Indet => Indet
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

let rec evaluate = (d: DHExp.t): result =>
  switch (d) {
  | BoundVar(x) => raise(EvaluatorError.Exception(FreeInvalidVar(x)))
  | Let(dp, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(d1)
    | Indet(d1) =>
      switch (matches(dp, d1)) {
      | Indet => Indet(d)
      | DoesNotMatch => Indet(d)
      | Matches(env) => evaluate(subst(env, d2))
      }
    }
  | TyAlias(_, _, d) => evaluate(d)
  | FixF(x, _, d1) => evaluate(subst_var(d, x, d1))
  | Lam(_, _, _) => BoxedValue(d)
  | Ap(d1, d2) =>
    let result = evaluate(d1);
    switch (result) {
    | BoxedValue(Lam(dp, _, d3)) =>
      switch (evaluate(d2)) {
      | BoxedValue(d2)
      | Indet(d2) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch => Indet(d)
        | Indet => Indet(d)
        | Matches(env) =>
          /* beta rule */
          evaluate(subst(env, d3))
        }
      }
    | BoxedValue(Cast(d1', (tyvars, _) as dty, (tyvars', _) as dty'))
    | Indet(Cast(d1', (tyvars, _) as dty, (tyvars', _) as dty')) =>
      switch (DHTyp.head_normalize(dty), DHTyp.head_normalize(dty')) {
      | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
        switch (evaluate(d2)) {
        | BoxedValue(d2')
        | Indet(d2') =>
          /* ap cast rule */
          evaluate(
            Cast(
              Ap(d1', Cast(d2', (tyvars', ty1'), (tyvars, ty1))),
              (tyvars, ty2),
              (tyvars', ty2'),
            ),
          )
        }
      | _ =>
        switch (result) {
        | BoxedValue(d1') =>
          raise(EvaluatorError.Exception(InvalidBoxedLam(d1')))
        | Indet(d1') =>
          switch (evaluate(d2)) {
          | BoxedValue(d2')
          | Indet(d2') => Indet(Ap(d1', d2'))
          }
        }
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedLam(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2'))
      }
    };
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => BoxedValue(d)
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => b3
      | None =>
        switch (evaluate(d2)) {
        | BoxedValue(BoolLit(b2)) =>
          BoxedValue(eval_bin_bool_op(op, b1, b2))
        | BoxedValue(d2') =>
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
        }
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(IntLit(n1) as d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(IntLit(n2)) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Indet(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              DivideByZero,
            ),
          )
        | _ => BoxedValue(eval_bin_int_op(op, n1, n2))
        }
      | BoxedValue(d2') =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(FloatLit(f1) as d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(FloatLit(f2)) =>
        BoxedValue(eval_bin_float_op(op, f1, f2))
      | BoxedValue(d2') =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    }
  | Inj(ty, side, d1) =>
    switch (evaluate(d1)) {
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1'))
    | Indet(d1') => Indet(Inj(ty, side, d1'))
    }
  | Pair(d1, d2) =>
    switch (evaluate(d1), evaluate(d2)) {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2))
    | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Pair(d1, d2))
    }
  | Cons(d1, d2) =>
    switch (evaluate(d1), evaluate(d2)) {
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
    switch (evaluate(d1)) {
    | BoxedValue(d1')
    | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
    }
  | FreeVar(_) => Indet(d)
  | Keyword(_) => Indet(d)
  | InvalidText(_) => Indet(d)
  | Cast(d1, dty, dty') =>
    switch (evaluate(d1)) {
    | BoxedValue(d1') as result =>
      switch (DHTyp.ground_cases_of(dty), DHTyp.ground_cases_of(dty')) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(d1', dty, dty'))
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', dty'', dty''') when DHTyp.is_hole(dty''') =>
          if (DHTyp.equivalent(dty'', dty')) {
            BoxedValue(d1'');
          } else {
            Indet(FailedCast(d1', dty, dty'));
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let dty'_grounded = DHTyp.wrap(HTyp.of_normalized(ty'_grounded));
        let d' =
          DHExp.Cast(Cast(d1', dty, dty'_grounded), dty'_grounded, dty');
        evaluate(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let dty_grounded = DHTyp.wrap(HTyp.of_normalized(ty_grounded));
        let d' =
          DHExp.Cast(Cast(d1', dty, dty_grounded), dty_grounded, dty');
        evaluate(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(d1', dty, dty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (DHTyp.equivalent(dty, dty')) {
          result;
        } else {
          BoxedValue(Cast(d1', dty, dty'));
        }
      }
    | Indet(d1') as result =>
      switch (DHTyp.ground_cases_of(dty), DHTyp.ground_cases_of(dty')) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(d1', dty, dty'))
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', dty'', dty''') when DHTyp.is_hole(dty''') =>
          if (DHTyp.equivalent(dty'', dty')) {
            Indet(d1'');
          } else {
            Indet(FailedCast(d1', dty, dty'));
          }
        | _ => Indet(Cast(d1', dty, dty'))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let dty'_grounded = DHTyp.wrap(HTyp.of_normalized(ty'_grounded));
        let d' =
          DHExp.Cast(Cast(d1', dty, dty'_grounded), dty'_grounded, dty');
        evaluate(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let dty_grounded = DHTyp.wrap(HTyp.of_normalized(ty_grounded));
        let d' =
          DHExp.Cast(Cast(d1', dty, dty_grounded), dty_grounded, dty');
        evaluate(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(d1', dty, dty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (DHTyp.equivalent(dty, dty')) {
          result;
        } else {
          Indet(Cast(d1', dty, dty'));
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (evaluate(d1)) {
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty'))
    }
  | InvalidOperation(d, err) => Indet(InvalidOperation(d, err))
  }
and evaluate_case =
    (
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : result =>
  switch (evaluate(scrut)) {
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
      switch (matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Matches(env) => evaluate(subst(env, d))
      | DoesNotMatch =>
        evaluate_case(inconsistent_info, scrut, rules, current_rule_index + 1)
      }
    }
  };
