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
      switch (Elaborator_Exp.matches(dp, d1)) {
      | Indet => (Indet(d), map1)
      | DoesNotMatch => (Indet(d), map1)
      | Matches(env) => evaluate(Elaborator_Exp.subst(env, d2), map1)
      }
    }
  | FixF(x, _, d1) =>
    evaluate(Elaborator_Exp.subst_var(d, x, d1), assert_map)
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
        switch (Elaborator_Exp.matches(dp, d2)) {
        | DoesNotMatch => (Indet(d), map2)
        | Indet => (Indet(d), map2)
        | Matches(env) =>
          /* beta rule */
          evaluate(Elaborator_Exp.subst(env, d3), map2)
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
      print_endline(
        Sexplib.Sexp.to_string(AssertMap.sexp_of_t(assert_map)),
      );
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
      switch (Elaborator_Exp.matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Matches(env) =>
        fst(evaluate(Elaborator_Exp.subst(env, d), assert_map))
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
