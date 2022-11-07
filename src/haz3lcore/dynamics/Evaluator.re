open Util;
open EvaluatorMonad;
open EvaluatorMonad.Syntax;
open EvaluatorResult;

/**
  Alias for EvaluatorMonad.
 */
type m('a) = EvaluatorMonad.t('a);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(Typ.t) /* the argument is the corresponding ground type */;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let grounded_Arrow =
  NotGroundOrHole(Arrow(Unknown(Internal), Unknown(Internal)));
// let grounded_Sum =
//   NotGroundOrHole(Sum(Unknown(Internal), Unknown(Internal)));
let grounded_Prod = length =>
  NotGroundOrHole(Prod(ListUtil.replicate(length, Typ.Unknown(Internal))));
let grounded_List = NotGroundOrHole(List(Unknown(Internal)));

let ground_cases_of = (ty: Typ.t): ground_cases =>
  switch (ty) {
  | Unknown(_) => Hole
  | Bool
  | Int
  | Float
  | String
  | Var(_) // TODO(andrew): ?
  | Arrow(Unknown(_), Unknown(_))
  // | Sum(Unknown(_), Unknown(_))
  | List(Unknown(_)) => Ground
  | Prod(tys) =>
    if (List.for_all(
          fun
          | Typ.Unknown(_) => true
          | _ => false,
          tys,
        )) {
      Ground;
    } else {
      tys |> List.length |> grounded_Prod;
    }
  | Arrow(_, _) => grounded_Arrow
  //  | Sum(_, _) => grounded_Sum
  | List(_) => grounded_List
  };

let rec matches = (dp: DHPat.t, d: DHExp.t): match_result =>
  switch (dp, d) {
  | (_, BoundVar(_)) => DoesNotMatch
  | (EmptyHole(_), _)
  | (NonEmptyHole(_), _) => IndetMatch
  | (Wild, _) => Matches(Environment.empty)
  | (ExpandingKeyword(_), _) => DoesNotMatch
  | (InvalidText(_), _) => IndetMatch
  | (Var(x), _) =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  | (_, EmptyHole(_)) => IndetMatch
  | (_, NonEmptyHole(_)) => IndetMatch
  | (_, FailedCast(_)) => IndetMatch
  | (_, InvalidOperation(_)) => IndetMatch
  | (_, FreeVar(_)) => IndetMatch
  | (_, InvalidText(_)) => IndetMatch
  | (_, Let(_)) => IndetMatch
  | (_, FixF(_)) => DoesNotMatch
  | (_, Fun(_)) => DoesNotMatch
  | (_, BinBoolOp(_)) => IndetMatch
  | (_, BinIntOp(_)) => IndetMatch
  | (_, BinFloatOp(_)) => IndetMatch
  | (_, ConsistentCase(Case(_))) => IndetMatch

  /* Closure should match like underlying expression. */
  | (_, Closure(_, d')) => matches(dp, d')

  | (BoolLit(b1), BoolLit(b2)) =>
    if (b1 == b2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (BoolLit(_), Cast(d, Bool, Unknown(_))) => matches(dp, d)
  | (BoolLit(_), Cast(d, Unknown(_), Bool)) => matches(dp, d)
  | (BoolLit(_), _) => DoesNotMatch
  | (IntLit(n1), IntLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (IntLit(_), Cast(d, Int, Unknown(_))) => matches(dp, d)
  | (IntLit(_), Cast(d, Unknown(_), Int)) => matches(dp, d)
  | (IntLit(_), _) => DoesNotMatch
  | (FloatLit(n1), FloatLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (FloatLit(_), Cast(d, Float, Unknown(_))) => matches(dp, d)
  | (FloatLit(_), Cast(d, Unknown(_), Float)) => matches(dp, d)
  | (FloatLit(_), _) => DoesNotMatch
  | (StringLit(s1), StringLit(s2)) =>
    if (s1 == s2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (StringLit(_), Cast(d, String, Unknown(_))) => matches(dp, d)
  | (StringLit(_), Cast(d, Unknown(_), String)) => matches(dp, d)
  | (StringLit(_), _) => DoesNotMatch
  | (Tag(n1), Tag(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (Tag(_), Cast(d, _, Unknown(_))) => matches(dp, d)
  | (Tag(_), Cast(d, Unknown(_), _)) => matches(dp, d)
  | (Tag(_), _) => DoesNotMatch
  | (Inj(side1, dp), Inj(_, side2, d)) =>
    switch (side1, side2) {
    | (L, L)
    | (R, R) => matches(dp, d)
    | _ => DoesNotMatch
    }
  | (Inj(side, dp), Cast(_)) => matches_cast_Inj(side, dp, d, [])
  | (Inj(_, _), _) => DoesNotMatch
  | (Tuple(dps), Tuple(ds)) =>
    if (List.length(dps) != List.length(ds)) {
      DoesNotMatch;
    } else {
      List.fold_left2(
        (result, dp, d) =>
          switch (result) {
          | DoesNotMatch => DoesNotMatch
          | IndetMatch => IndetMatch
          | Matches(env) =>
            switch (matches(dp, d)) {
            | DoesNotMatch => DoesNotMatch
            | IndetMatch => IndetMatch
            | Matches(env') => Matches(Environment.union(env, env'))
            }
          },
        Matches(Environment.empty),
        dps,
        ds,
      );
    }
  | (Ap(dp1, dp2), Ap(d1, d2)) =>
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
  | (Tuple(dps), Cast(d, Prod(tys), Prod(tys'))) =>
    matches_cast_Tuple(dps, d, [List.combine(tys, tys')])
  | (Tuple(dps), Cast(d, Prod(tys), Unknown(_))) =>
    matches_cast_Tuple(
      dps,
      d,
      [
        List.combine(
          tys,
          List.init(List.length(tys), _ => Typ.Unknown(Internal)),
        ),
      ],
    )
  | (Tuple(dps), Cast(d, Unknown(_), Prod(tys'))) =>
    matches_cast_Tuple(
      dps,
      d,
      [
        List.combine(
          List.init(List.length(tys'), _ => Typ.Unknown(Internal)),
          tys',
        ),
      ],
    )
  | (Tuple(_), Cast(_)) => DoesNotMatch
  | (Tuple(_), _) => DoesNotMatch
  | (Cons(_) | ListLit(_), Cast(d, List(ty1), List(ty2))) =>
    matches_cast_Cons(dp, d, [(ty1, ty2)])
  | (Cons(_) | ListLit(_), Cast(d, Unknown(_), List(ty2))) =>
    matches_cast_Cons(dp, d, [(Unknown(Internal), ty2)])
  | (Cons(_) | ListLit(_), Cast(d, List(ty1), Unknown(_))) =>
    matches_cast_Cons(dp, d, [(ty1, Unknown(Internal))])
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
      casts: list((Typ.t, Typ.t, Typ.t, Typ.t)),
    )
    : match_result =>
  switch (d) {
  | Inj(_, side', d') =>
    switch (side, side') {
    | (L, L)
    | (R, R) =>
      let side_casts =
        List.map(
          (c: (Typ.t, Typ.t, Typ.t, Typ.t)) => {
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
  // | Cast(d', Sum(tyL1, tyR1), Sum(tyL2, tyR2)) =>
  //   matches_cast_Inj(side, dp, d', [(tyL1, tyR1, tyL2, tyR2), ...casts])
  // | Cast(d', Sum(tyL1, tyR1), Unknown(_)) =>
  //   matches_cast_Inj(
  //     side,
  //     dp,
  //     d',
  //     [(tyL1, tyR1, Unknown(Internal), Unknown(Internal))],
  //   )
  // | Cast(d', Unknown(_), Sum(tyL2, tyR2)) =>
  //   matches_cast_Inj(
  //     side,
  //     dp,
  //     d',
  //     [(Unknown(Internal), Unknown(Internal), tyL2, tyR2)],
  //   )
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_) => IndetMatch
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
  | BinStringOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | TestLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | StringLit(_) => DoesNotMatch
  | ListLit(_, _, _, _, _) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Tuple(_) => DoesNotMatch
  | Prj(_) => DoesNotMatch
  | Tag(_) => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_) => IndetMatch
  | NonEmptyHole(_) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  }
and matches_cast_Tuple =
    (
      dps: list(DHPat.t),
      d: DHExp.t,
      elt_casts: list(list((Typ.t, Typ.t))),
    )
    : match_result =>
  switch (d) {
  | Tuple(ds) =>
    if (List.length(dps) != List.length(ds)) {
      DoesNotMatch;
    } else {
      List.fold_right(
        (((dp, d), casts), result) => {
          switch (result) {
          | DoesNotMatch
          | IndetMatch => result
          | Matches(env) =>
            switch (matches(dp, DHExp.apply_casts(d, casts))) {
            | DoesNotMatch => DoesNotMatch
            | IndetMatch => IndetMatch
            | Matches(env') => Matches(Environment.union(env, env'))
            }
          }
        },
        List.combine(List.combine(dps, ds), elt_casts),
        Matches(Environment.empty),
      );
    }
  | Cast(d', Prod(tys), Prod(tys')) =>
    if (List.length(dps) != List.length(tys)) {
      DoesNotMatch;
    } else {
      matches_cast_Tuple(dps, d', [List.combine(tys, tys'), ...elt_casts]);
    }
  | Cast(d', Prod(tys), Unknown(_)) =>
    let tys' = List.init(List.length(tys), _ => Typ.Unknown(Internal));
    matches_cast_Tuple(dps, d', [List.combine(tys, tys'), ...elt_casts]);
  | Cast(d', Unknown(_), Prod(tys')) =>
    let tys = List.init(List.length(tys'), _ => Typ.Unknown(Internal));
    matches_cast_Tuple(dps, d', [List.combine(tys, tys'), ...elt_casts]);
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_) => IndetMatch
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
  | BinStringOp(_)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | TestLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | StringLit(_) => DoesNotMatch
  | Inj(_, _, _) => DoesNotMatch
  | ListLit(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Prj(_) => DoesNotMatch
  | Tag(_) => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_) => IndetMatch
  | NonEmptyHole(_) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  }
and matches_cast_Cons =
    (dp: DHPat.t, d: DHExp.t, elt_casts: list((Typ.t, Typ.t))): match_result =>
  switch (d) {
  | ListLit(_, _, _, _, []) =>
    switch (dp) {
    | ListLit(_, []) => Matches(Environment.empty)
    | _ => DoesNotMatch
    }
  | ListLit(u, i, err, ty, [dhd, ...dtl] as ds) =>
    switch (dp) {
    | Cons(dp1, dp2) =>
      switch (matches(dp1, DHExp.apply_casts(dhd, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | IndetMatch => IndetMatch
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (Typ.t, Typ.t)) => {
              let (ty1, ty2) = c;
              (Typ.List(ty1), Typ.List(ty2));
            },
            elt_casts,
          );
        let d2 = DHExp.ListLit(u, i, err, ty, dtl);
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
            (c: (Typ.t, Typ.t)) => {
              let (ty1, ty2) = c;
              (Typ.List(ty1), Typ.List(ty2));
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
            (c: (Typ.t, Typ.t)) => {
              let (ty1, ty2) = c;
              (Typ.List(ty1), Typ.List(ty2));
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
  | Cast(d', List(ty1), Unknown(_)) =>
    matches_cast_Cons(dp, d', [(ty1, Unknown(Internal)), ...elt_casts])
  | Cast(d', Unknown(_), List(ty2)) =>
    matches_cast_Cons(dp, d', [(Unknown(Internal), ty2), ...elt_casts])
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_) => IndetMatch
  | InvalidText(_) => IndetMatch
  | ExpandingKeyword(_) => IndetMatch
  | Let(_, _, _) => IndetMatch
  | FixF(_, _, _) => DoesNotMatch
  | Fun(_, _, _) => DoesNotMatch
  | Closure(_, d') => matches_cast_Cons(dp, d', elt_casts)
  | Ap(_, _) => IndetMatch
  | ApBuiltin(_, _) => IndetMatch
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BinStringOp(_)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | Sequence(_)
  | TestLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | StringLit(_) => DoesNotMatch
  | Inj(_, _, _) => DoesNotMatch
  | Tuple(_) => DoesNotMatch
  | Prj(_) => DoesNotMatch
  | Tag(_) => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => IndetMatch
  | EmptyHole(_) => IndetMatch
  | NonEmptyHole(_) => IndetMatch
  | FailedCast(_, _, _) => IndetMatch
  | InvalidOperation(_) => IndetMatch
  };

/**
  [eval_bin_bool_op op b1 b2] is the result of applying [op] to [b1] and [b2].
 */
let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
  switch (op) {
  | And => BoolLit(b1 && b2)
  | Or => BoolLit(b1 || b2)
  };

/**
  [eval_bin_bool_op_short_circuit op b1] is [Some b] if [op b1 b2] can be
  resolved with just [b1].
 */
let eval_bin_bool_op_short_circuit =
    (op: DHExp.BinBoolOp.t, b1: bool): option(DHExp.t) =>
  switch (op, b1) {
  | (Or, true) => Some(BoolLit(true))
  | (And, false) => Some(BoolLit(false))
  | _ => None
  };

/**
  [eval_bin_int_op op n1 n2] is the result of applying [op] to [n1] and [n2].
 */
let eval_bin_int_op = (op: DHExp.BinIntOp.t, n1: int, n2: int): DHExp.t => {
  switch (op) {
  | Minus => IntLit(n1 - n2)
  | Plus => IntLit(n1 + n2)
  | Times => IntLit(n1 * n2)
  | Divide => IntLit(n1 / n2)
  | LessThan => BoolLit(n1 < n2)
  | LessThanOrEqual => BoolLit(n1 <= n2)
  | GreaterThan => BoolLit(n1 > n2)
  | GreaterThanOrEqual => BoolLit(n1 >= n2)
  | Equals => BoolLit(n1 == n2)
  };
};

/**
  [eval_bin_float_op op f1 f2] is the result of applying [op] to [f1] and [f2].
 */
let eval_bin_float_op =
    (op: DHExp.BinFloatOp.t, f1: float, f2: float): DHExp.t => {
  switch (op) {
  | FPlus => FloatLit(f1 +. f2)
  | FMinus => FloatLit(f1 -. f2)
  | FTimes => FloatLit(f1 *. f2)
  | FPower => FloatLit(f1 ** f2)
  | FDivide => FloatLit(f1 /. f2)
  | FLessThan => BoolLit(f1 < f2)
  | FLessThanOrEqual => BoolLit(f1 <= f2)
  | FGreaterThan => BoolLit(f1 > f2)
  | FGreaterThanOrEqual => BoolLit(f1 >= f2)
  | FEquals => BoolLit(f1 == f2)
  };
};

let eval_bin_string_op =
    (op: DHExp.BinStringOp.t, s1: string, s2: string): DHExp.t =>
  switch (op) {
  | SEquals => BoolLit(s1 == s2)
  };

let rec evaluate: (ClosureEnvironment.t, DHExp.t) => m(EvaluatorResult.t) =
  (env, d) => {
    /* Increment number of evaluation steps (calls to `evaluate`). */
    let* () = take_step;

    switch (d) {
    | BoundVar(x) =>
      let d =
        x
        |> ClosureEnvironment.lookup(env)
        |> OptUtil.get(() => {
             print_endline("FreeInvalidVar");
             raise(EvaluatorError.Exception(FreeInvalidVar(x)));
           });
      /* We need to call [evaluate] on [d] again since [env] does not store
       * final expressions. */
      evaluate(env, d);

    | Sequence(d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(_d1) => evaluate(env, d2)
      /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
       * if evaluated d1 is indet. */
      | Indet(_d1) =>
        /* let* r2 = evaluate(env, d2); */
        /* switch (r2) { */
        /* | BoxedValue(d2) */
        /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
        /* }; */
        evaluate(env, d2)
      };

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

    | FixF(f, _, d') =>
      let* env' = evaluate_extend_env(Environment.singleton((f, d)), env);
      evaluate(env', d');

    | Fun(_) => BoxedValue(Closure(env, d)) |> return

    | Ap(d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(TestLit(id)) => evaluate_test(env, id, d2)
      | BoxedValue(Tag(_)) =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
        | Indet(d2) => Indet(Ap(d1, d2)) |> return
        };
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
        print_endline("InvalidBoxedFun");
        raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(Ap(d1', d2')) |> return
        };
      };

    | ApBuiltin(ident, args) => evaluate_ap_builtin(env, ident, args)

    | TestLit(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | Tag(_) => BoxedValue(d) |> return

    | BinBoolOp(op, d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (eval_bin_bool_op_short_circuit(op, b1)) {
        | Some(b3) => BoxedValue(b3) |> return
        | None =>
          let* r2 = evaluate(env, d2);
          switch (r2) {
          | BoxedValue(BoolLit(b2)) =>
            BoxedValue(eval_bin_bool_op(op, b1, b2)) |> return
          | BoxedValue(d2') =>
            print_endline("InvalidBoxedBoolLit");
            raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')));
          | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
          };
        }
      | BoxedValue(d1') =>
        print_endline("InvalidBoxedBoolLit");
        raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')));
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
          print_endline("InvalidBoxedIntLit1");
          print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d2')));
          raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')));
        | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
        };
      | BoxedValue(d1') =>
        print_endline("InvalidBoxedIntLit2");
        print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d1')));
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')));
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
          print_endline("InvalidBoxedFloatLit");
          raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')));
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
        };
      | BoxedValue(d1') =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')));
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
        };
      };

    | BinStringOp(op, d1, d2) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(StringLit(f1) as d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(StringLit(f2)) =>
          BoxedValue(eval_bin_string_op(op, f1, f2)) |> return
        | BoxedValue(d2') =>
          print_endline("InvalidBoxedStringLit");
          raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')));
        | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
        };
      | BoxedValue(d1') =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')));
      | Indet(d1') =>
        let* r2 = evaluate(env, d2);
        switch (r2) {
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
        };
      };

    | Inj(ty, side, d1) =>
      let* r1 = evaluate(env, d1);
      switch (r1) {
      | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1')) |> return
      | Indet(d1') => Indet(Inj(ty, side, d1')) |> return
      };

    | Tuple(ds) =>
      let+ lst = ds |> List.map(evaluate(env)) |> sequence;
      let (ds', indet) =
        List.fold_right(
          (el, (lst, indet)) =>
            switch (el) {
            | BoxedValue(el) => ([el, ...lst], false || indet)
            | Indet(el) => ([el, ...lst], true)
            },
          lst,
          ([], false),
        );

      let d = DHExp.Tuple(ds');
      if (indet) {
        Indet(d);
      } else {
        BoxedValue(d);
      };

    | Prj(targ, n) =>
      if (n < 0) {
        return(
          Indet(
            InvalidOperation(d, InvalidOperationError.InvalidProjection),
          ),
        );
      } else {
        let* r = evaluate(env, targ);
        switch (r) {
        | BoxedValue(Tuple(ds) as rv) =>
          if (n >= List.length(ds)) {
            return(
              Indet(
                InvalidOperation(rv, InvalidOperationError.InvalidProjection),
              ),
            );
          } else {
            return(BoxedValue(List.nth(ds, n)));
          }
        | Indet(Tuple(ds) as rv) =>
          if (n >= List.length(ds)) {
            return(
              Indet(
                InvalidOperation(rv, InvalidOperationError.InvalidProjection),
              ),
            );
          } else {
            return(Indet(List.nth(ds, n)));
          }
        | BoxedValue(Cast(targ', Prod(tys), Prod(tys')) as rv)
        | Indet(Cast(targ', Prod(tys), Prod(tys')) as rv) =>
          if (n >= List.length(tys)) {
            return(
              Indet(
                InvalidOperation(rv, InvalidOperationError.InvalidProjection),
              ),
            );
          } else {
            let ty = List.nth(tys, n);
            let ty' = List.nth(tys', n);
            evaluate(env, Cast(Prj(targ', n), ty, ty'));
          }
        | _ => return(Indet(d))
        };
      }
    | Cons(d1, d2) =>
      let* d1 = evaluate(env, d1);
      let* d2 = evaluate(env, d2);
      switch (d1, d2) {
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2)) |> return
      | (BoxedValue(d1), BoxedValue(d2)) =>
        switch (d2) {
        | ListLit(x1, x2, x3, x4, lst) =>
          BoxedValue(ListLit(x1, x2, x3, x4, [d1, ...lst])) |> return
        | Cast(ListLit(x1, x2, x3, x4, lst), List(ty), List(ty')) =>
          BoxedValue(
            Cast(
              ListLit(x1, x2, x3, x4, [d1, ...lst]),
              List(ty),
              List(ty'),
            ),
          )
          |> return
        | _ =>
          print_endline("InvalidBoxedListLit");
          raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
        }
      };

    | ListLit(u, i, err, ty, lst) =>
      let+ lst = lst |> List.map(evaluate(env)) |> sequence;
      let (lst, indet) =
        List.fold_right(
          (el, (lst, indet)) =>
            switch (el) {
            | BoxedValue(el) => ([el, ...lst], false || indet)
            | Indet(el) => ([el, ...lst], true)
            },
          lst,
          ([], false),
        );

      let d = DHExp.ListLit(u, i, err, ty, lst);
      if (indet) {
        Indet(d);
      } else {
        BoxedValue(d);
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
          | Cast(d1'', ty'', Unknown(_)) =>
            if (Typ.eq(ty'', ty')) {
              BoxedValue(d1'') |> return;
            } else {
              Indet(FailedCast(d1', ty, ty')) |> return;
            }
          | _ =>
            print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d1)));
            print_endline(Sexplib.Sexp.to_string_hum(Typ.sexp_of_t(ty)));
            print_endline(Sexplib.Sexp.to_string_hum(Typ.sexp_of_t(ty')));
            print_endline("CastBVHoleGround");
            raise(EvaluatorError.Exception(CastBVHoleGround(d1')));
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
          if (Typ.eq(ty, ty')) {
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
          | Cast(d1'', ty'', Unknown(_)) =>
            if (Typ.eq(ty'', ty')) {
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
          if (Typ.eq(ty, ty')) {
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

/**
  [evaluate_case env inconsistent_info scrut rules current_rule_index]
  evaluates a case expression.
 */
and evaluate_case =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : m(EvaluatorResult.t) => {
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

/**
  [evaluate_extend_env env' env] extends [env] with bindings from [env'].
 */
and evaluate_extend_env =
    (new_bindings: Environment.t, to_extend: ClosureEnvironment.t)
    : m(ClosureEnvironment.t) => {
  let map =
    Environment.union(new_bindings, ClosureEnvironment.map_of(to_extend));
  map |> ClosureEnvironment.of_environment |> with_eig;
}

/**
  [evaluate_ap_builtin env ident args] evaluates the builtin function given by
  [ident] with [args].
 */
and evaluate_ap_builtin =
    (env: ClosureEnvironment.t, ident: string, args: list(DHExp.t))
    : m(EvaluatorResult.t) => {
  switch (VarMap.lookup(Builtins.forms(Builtins.Pervasives.builtins), ident)) {
  | Some((_, eval)) => eval(env, args, evaluate)
  | None =>
    print_endline("InvalidBuiltin");
    raise(EvaluatorError.Exception(InvalidBuiltin(ident)));
  };
}

and evaluate_test =
    (env: ClosureEnvironment.t, n: KeywordID.t, arg: DHExp.t)
    : m(EvaluatorResult.t) => {
  let* (arg_show, arg_result) =
    switch (DHExp.strip_casts(arg)) {
    | BinBoolOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinBoolOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2);
    | BinIntOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinIntOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2);
    | BinFloatOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinFloatOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2);

    | Ap(Ap(arg_d1, arg_d2), arg_d3) =>
      let* arg_d1 = evaluate(env, arg_d1);
      let* arg_d2 = evaluate(env, arg_d2);
      let* arg_d3 = evaluate(env, arg_d3);
      let arg_show =
        DHExp.Ap(
          Ap(EvaluatorResult.unbox(arg_d1), EvaluatorResult.unbox(arg_d2)),
          EvaluatorResult.unbox(arg_d3),
        );
      let* arg_result = evaluate(env, arg_show);
      (arg_show, arg_result) |> return;

    | Ap(arg_d1, arg_d2) =>
      let mk = (arg_d1, arg_d2) => DHExp.Ap(arg_d1, arg_d2);
      evaluate_test_eq(env, mk, arg_d1, arg_d2);

    | _ =>
      let* arg = evaluate(env, arg);
      (EvaluatorResult.unbox(arg), arg) |> return;
    };

  let test_status: TestStatus.t =
    switch (arg_result) {
    | BoxedValue(BoolLit(true)) => Pass
    | BoxedValue(BoolLit(false)) => Fail
    | _ => Indet
    };

  let* _ = add_test(n, (arg_show, test_status));
  let r: EvaluatorResult.t =
    switch (arg_result) {
    | BoxedValue(BoolLit(_)) => BoxedValue(Tuple([]))
    | BoxedValue(arg)
    | Indet(arg) => Indet(Ap(TestLit(n), arg))
    };
  r |> return;
}

and evaluate_test_eq =
    (
      env: ClosureEnvironment.t,
      mk_arg_op: (DHExp.t, DHExp.t) => DHExp.t,
      arg_d1: DHExp.t,
      arg_d2: DHExp.t,
    )
    : m((DHExp.t, EvaluatorResult.t)) => {
  let* arg_d1 = evaluate(env, arg_d1);
  let* arg_d2 = evaluate(env, arg_d2);

  let arg_show =
    mk_arg_op(EvaluatorResult.unbox(arg_d1), EvaluatorResult.unbox(arg_d2));
  let* arg_result = evaluate(env, arg_show);

  (arg_show, arg_result) |> return;
};

let evaluate = (env, d) => {
  let es = EvaluatorState.init;
  let (env, es) =
    es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env));
  evaluate(env, d, es);
};
