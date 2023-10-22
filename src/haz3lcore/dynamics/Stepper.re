open Util;

open StepperResult;
open DHExp;
open EvaluatorMonad;
open EvaluatorMonad.Syntax;

type evaluator =
  (~env: ClosureEnvironment.t=?, ~fenv: FilterEnvironment.t=?, DHExp.t) =>
  EvaluatorMonad.t(StepperResult.t(DHExp.t));

type environment = {
  env: ClosureEnvironment.t,
  fenv: FilterEnvironment.t,
  require: evaluator,
  evaluate: evaluator,
  builtin: evaluator,
  continue: evaluator,
};

let transition =
    (~env: environment, d: DHExp.t)
    : EvaluatorMonad.t(StepperResult.t(DHExp.t)) => {
  let {env, fenv, require, evaluate, builtin, continue} = env;
  switch (d) {
  | BoundVar(x) =>
    let d =
      x
      |> ClosureEnvironment.lookup(env)
      |> OptUtil.get(() => {
           print_endline("FreeInvalidVar: " ++ x);
           raise(EvaluatorError.Exception(FreeInvalidVar(x)));
         });
    BoxedValue(d) |> return;

  | Sequence(d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) => Expr(Sequence(d1, d2)) |> return
    | BoxedValue(_)
    | Indet(_) => continue(d2)
    };

  | Let(dp, d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) => Expr(Let(dp, d1, d2)) |> return
    | BoxedValue(d1)
    | Indet(d1) =>
      switch (Evaluator.matches(dp, d1)) {
      | IndetMatch
      | DoesNotMatch => Indet(Let(dp, d1, d2)) |> return
      | Matches(env') =>
        let* env = env |> Evaluator.evaluate_extend_env(env');
        continue(~env, d2);
      }
    };

  | Filter(filter, d1) =>
    let fenv = FilterEnvironment.extends(filter, fenv);
    continue(~fenv, d1);

  | FixF(f, _, d) =>
    let* env =
      env |> Evaluator.evaluate_extend_env(Environment.singleton((f, d)));
    continue(~env, d);

  | Fun(_) => BoxedValue(Closure(env, fenv, d)) |> return

  | Ap(d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2)
      | Indet(d2)
      | BoxedValue(d2) => Expr(Ap(d1, d2)) |> return
      };
    | BoxedValue(TestLit(id)) =>
      let* r2 = Evaluator.evaluate_test(env, id, d2);
      switch (r2) {
      | Indet(r2) => Indet(r2) |> return
      | BoxedValue(r2) => BoxedValue(r2) |> return
      };
    | BoxedValue(Constructor(_) as d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(Ap(d1, d2)) |> return
      | Indet(d2) => Indet(Ap(d1, d2)) |> return
      | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
      };
    | BoxedValue(Closure(env, fenv, Fun(dp, _, d3, _)) as d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(Ap(d1, d2)) |> return
      | BoxedValue(_)
      | Indet(_) =>
        switch (Evaluator.matches(dp, d2)) {
        | DoesNotMatch
        | IndetMatch => Indet(Ap(d1, d2)) |> return
        | Matches(env') =>
          /* evaluate a closure: extend the closure environment with the
           * new bindings introduced by the function application. */
          let* env = env |> Evaluator.evaluate_extend_env(env');
          continue(~env, ~fenv, d3);
        }
      };
    | BoxedValue(Cast(d1, Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1, Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(Ap(d1, d2)) |> return
      | Indet(d2)
      | BoxedValue(d2) =>
        /* ap cast rule */
        continue(Cast(Ap(d1, Cast(d2, ty1', ty1)), ty2, ty2'))
      };
    | BoxedValue(d1) =>
      print_endline("InvalidBoxedFun");
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1)));
    | Indet(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(Ap(d1, d2)) |> return
      | Indet(d2)
      | BoxedValue(d2) => Indet(Ap(d1, d2)) |> return
      };
    };

  | ApBuiltin(ident, args) =>
    let* r = Evaluator.evaluate_ap_builtin(env, ident, args);
    switch (r) {
    | Indet(d) => Indet(d) |> return
    | BoxedValue(d) => builtin(d)
    };

  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Constructor(_) => BoxedValue(d) |> return

  | BinBoolOp(op, d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2)
      | Indet(d2)
      | BoxedValue(d2) => Expr(BinBoolOp(op, d1, d2)) |> return
      };
    | BoxedValue(BoolLit(b1) as d1) =>
      switch (Evaluator.eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => builtin(b3)
      | None =>
        let* r2 = require(d2);
        switch (r2) {
        | Expr(d2) => Expr(BinBoolOp(op, d1, d2)) |> return
        | Indet(d2) => Indet(BinBoolOp(op, d1, d2)) |> return
        | BoxedValue(BoolLit(b2)) =>
          builtin(Evaluator.eval_bin_bool_op(op, b1, b2))
        | BoxedValue(d2) =>
          print_endline("InvalidBoxedBoolLit");
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2)));
        };
      }
    | BoxedValue(d1) =>
      print_endline("InvalidBoxedBoolLit");
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)));
    | Indet(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinBoolOp(op, d1, d2)) |> return
      | Indet(d2)
      | BoxedValue(d2) => Indet(BinBoolOp(op, d1, d2)) |> return
      };
    };

  | BinIntOp(op, d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2)
      | Indet(d2)
      | BoxedValue(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      };
    | BoxedValue(IntLit(n1) as d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinIntOp(op, d1, d2)) |> return
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
        | (Power, _, _) when n2 < 0 =>
          Indet(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              NegativeExponent,
            ),
          )
          |> return
        | _ => builtin(Evaluator.eval_bin_int_op(op, n1, n2))
        }
      | BoxedValue(d2) =>
        print_endline("InvalidBoxedIntLit1");
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2)));
      | Indet(d2) => Indet(BinIntOp(op, d1, d2)) |> return
      };
    | BoxedValue(d1) =>
      print_endline("InvalidBoxedIntLit2");
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)));
    | Indet(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      | BoxedValue(d2)
      | Indet(d2) => Indet(BinIntOp(op, d1, d2)) |> return
      };
    };

  | BinFloatOp(op, d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2)
      | Indet(d2)
      | BoxedValue(d2) => Expr(BinFloatOp(op, d1, d2)) |> return
      };
    | BoxedValue(FloatLit(f1) as d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinFloatOp(op, d1, d2)) |> return
      | BoxedValue(FloatLit(f2)) =>
        builtin(Evaluator.eval_bin_float_op(op, f1, f2))
      | BoxedValue(d2) =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2)));
      | Indet(d2) => Indet(BinFloatOp(op, d1, d2)) |> return
      };
    | BoxedValue(d1) =>
      print_endline("InvalidBoxedFloatLit");
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)));
    | Indet(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinFloatOp(op, d1, d2)) |> return
      | BoxedValue(d2)
      | Indet(d2) => Indet(BinFloatOp(op, d1, d2)) |> return
      };
    };

  | BinStringOp(op, d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2)
      | Indet(d2)
      | BoxedValue(d2) => Expr(BinStringOp(op, d1, d2)) |> return
      };
    | BoxedValue(StringLit(s1) as d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinStringOp(op, d1, d2)) |> return
      | BoxedValue(StringLit(s2)) =>
        builtin(Evaluator.eval_bin_string_op(op, s1, s2))
      | BoxedValue(d2) =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2)));
      | Indet(d2) => Indet(BinStringOp(op, d1, d2)) |> return
      };
    | BoxedValue(d1) =>
      print_endline("InvalidBoxedStringLit");
      raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1)));
    | Indet(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(BinStringOp(op, d1, d2)) |> return
      | BoxedValue(d2)
      | Indet(d2) => Indet(BinStringOp(op, d1, d2)) |> return
      };
    };

  | ListConcat(d1, d2) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2)
      | Indet(d2)
      | BoxedValue(d2) => Expr(ListConcat(d1, d2)) |> return
      };
    | BoxedValue(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(ListConcat(d1, d2)) |> return
      | BoxedValue(d2') =>
        switch (d1, d2') {
        | (ListLit(u, i, ty, ds1), ListLit(_, _, _, ds2)) =>
          builtin(ListLit(u, i, ty, ds1 @ ds2))
        | (Cast(d1, List(ty), List(ty')), d2)
        | (d1, Cast(d2, List(ty), List(ty'))) =>
          evaluate(Cast(ListConcat(d1, d2), List(ty), List(ty')))
        | (ListLit(_), _) =>
          print_endline("InvalidBoxedListLit: " ++ DHExp.show(d2));
          raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
        | _ =>
          print_endline("InvalidBoxedListLit: " ++ DHExp.show(d1));
          raise(EvaluatorError.Exception(InvalidBoxedListLit(d1)));
        }
      | Indet(d2) => Indet(ListConcat(d1, d2)) |> return
      };
    | Indet(d1) =>
      let* r2 = require(d2);
      switch (r2) {
      | Expr(d2) => Expr(ListConcat(d1, d2)) |> return
      | Indet(d2)
      | BoxedValue(d2) => Indet(ListConcat(d1, d2)) |> return
      };
    };

  | Tuple(ds) =>
    let+ drs = ds |> List.map(d => require(d) >>| (r => (d, r))) |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Expr(_), _) => (Expr(empty), [el, ...dst])
          | (_, Expr(el')) => (Expr(empty), [el', ...dst])
          | (Indet(_), _) => (Indet(empty), [el, ...dst])
          | (_, Indet(el')) => (Indet(empty), [el', ...dst])
          | (BoxedValue(_), BoxedValue(el')) => (
              BoxedValue(empty),
              [el', ...dst],
            )
          }
        },
        drs,
        (BoxedValue(empty), []),
      );
    let d' = DHExp.Tuple(ds');

    switch (tag) {
    | Expr(_) => Expr(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | Prj(targ, n) =>
    if (n < 0) {
      Indet(InvalidOperation(d, InvalidOperationError.InvalidProjection))
      |> return;
    } else {
      let* r = require(targ);
      switch (r) {
      | Expr(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          Expr(InvalidOperation(rv, InvalidOperationError.InvalidProjection))
          |> return;
        } else {
          Expr(List.nth(ds, n)) |> return;
        }
      | BoxedValue(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          Indet(
            InvalidOperation(rv, InvalidOperationError.InvalidProjection),
          )
          |> return;
        } else {
          builtin(List.nth(ds, n));
        }
      | Indet(Closure(_, _, Tuple(ds)) as rv)
      | Indet(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          Indet(
            InvalidOperation(rv, InvalidOperationError.InvalidProjection),
          )
          |> return;
        } else {
          Indet(List.nth(ds, n)) |> return;
        }
      | Expr(Cast(targ', Prod(tys), Prod(tys')) as rv)
      | BoxedValue(Cast(targ', Prod(tys), Prod(tys')) as rv)
      | Indet(Cast(targ', Prod(tys), Prod(tys')) as rv) =>
        if (n >= List.length(tys)) {
          Indet(
            InvalidOperation(rv, InvalidOperationError.InvalidProjection),
          )
          |> return;
        } else {
          let ty = List.nth(tys, n);
          let ty' = List.nth(tys', n);
          evaluate(Cast(Prj(targ', n), ty, ty'));
        }
      | Expr(d) => Expr(Prj(d, n)) |> return
      | _ => Indet(d) |> return
      };
    }

  | Cons(d1, d2) =>
    let* r1 = require(d1);
    let* r2 = require(d2);
    switch (r1, r2) {
    | (Expr(d1), Expr(d2))
    | (Expr(d1), Indet(d2))
    | (Expr(d1), BoxedValue(d2))
    | (Indet(d1), Expr(d2))
    | (BoxedValue(d1), Expr(d2)) => Expr(Cons(d1, d2)) |> return
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2)) |> return
    | (BoxedValue(d1), BoxedValue(d2)) =>
      switch (d2) {
      | ListLit(u, i, ty, ds) => builtin(ListLit(u, i, ty, [d1, ...ds]))
      | Cons(_)
      | Cast(ListLit(_), List(_), List(_)) =>
        BoxedValue(Cons(d1, d2)) |> return
      | _ =>
        print_endline("InvalidBoxedListLit");
        raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
      }
    };

  | ListLit(u, i, ty, ds) =>
    let+ drs = ds |> List.map(d => require(d) >>| (r => (d, r))) |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Expr(_), _) => (Expr(empty), [el, ...dst])
          | (_, Expr(el')) => (Expr(empty), [el', ...dst])
          | (Indet(_), _) => (Indet(empty), [el, ...dst])
          | (_, Indet(el')) => (Indet(empty), [el', ...dst])
          | (BoxedValue(_), BoxedValue(el')) => (
              BoxedValue(empty),
              [el', ...dst],
            )
          }
        },
        drs,
        (BoxedValue(empty), []),
      );

    let d' = DHExp.ListLit(u, i, ty, ds');

    switch (tag) {
    | Expr(_) => Expr(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | ConsistentCase(Case(_)) => failwith("todo")

  /* Generalized closures evaluate to themselves. Only
     lambda closures are BoxedValues; other closures are all Indet. */
  | Closure(env, fenv, d) =>
    switch (d) {
    | Fun(_) => BoxedValue(Closure(env, fenv, d)) |> return
    | d => evaluate(~env, ~fenv, d)
    }

  /* Hole expressions */
  | InconsistentBranches(u, i, Case(d1, rules, n)) =>
    //TODO: revisit this, consider some kind of dynamic casting
    Indet(
      Closure(env, fenv, InconsistentBranches(u, i, Case(d1, rules, n))),
    )
    |> return

  | EmptyHole(u, i) => Indet(Closure(env, fenv, EmptyHole(u, i))) |> return

  | NonEmptyHole(reason, u, i, d1) =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1') =>
      Expr(Closure(env, fenv, NonEmptyHole(reason, u, i, d1'))) |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      Indet(Closure(env, fenv, NonEmptyHole(reason, u, i, d1'))) |> return
    };

  | FreeVar(u, i, x) =>
    Indet(Closure(env, fenv, FreeVar(u, i, x))) |> return

  | ExpandingKeyword(u, i, kw) =>
    Indet(Closure(env, fenv, ExpandingKeyword(u, i, kw))) |> return

  | InvalidText(u, i, text) =>
    Indet(Closure(env, fenv, InvalidText(u, i, text))) |> return

  /* Cast calculus */
  | Cast(d1, ty, ty') =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1) => Expr(Cast(d1, ty, ty')) |> return
    | BoxedValue(d1) as result =>
      switch (Evaluator.ground_cases_of(ty), Evaluator.ground_cases_of(ty')) {
      | (Hole, Hole) => result |> return
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result |> return
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(d1, ty, ty')) |> return
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1) {
        | Cast(d1'', ty'', Unknown(_)) =>
          if (Typ.eq(ty'', ty')) {
            BoxedValue(d1'') |> return;
          } else {
            Indet(FailedCast(d1, ty, ty')) |> return;
          }
        | _ =>
          print_endline("CastBVHoleGround");
          raise(EvaluatorError.Exception(CastBVHoleGround(d1)));
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1, ty, ty'_grounded), ty'_grounded, ty');
        require(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1, ty, ty_grounded), ty_grounded, ty');
        require(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(d1, ty, ty')) |> return
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (Typ.eq(ty, ty')) {
          result |> return;
        } else {
          BoxedValue(Cast(d1, ty, ty')) |> return;
        }
      }
    | Indet(d1) as result =>
      switch (Evaluator.ground_cases_of(ty), Evaluator.ground_cases_of(ty')) {
      | (Hole, Hole) => result |> return
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result |> return
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(d1, ty, ty')) |> return
      | (Hole, Ground) =>
        switch (d1) {
        | Cast(d1'', ty'', Unknown(_)) =>
          if (Typ.eq(ty'', ty')) {
            Indet(d1'') |> return;
          } else {
            Indet(FailedCast(d1, ty, ty')) |> return;
          }
        | _ => Indet(Cast(d1, ty, ty')) |> return
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1, ty, ty'_grounded), ty'_grounded, ty');
        require(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1, ty, ty_grounded), ty_grounded, ty');
        require(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(d1, ty, ty')) |> return
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (Typ.eq(ty, ty')) {
          result |> return;
        } else {
          Indet(Cast(d1, ty, ty')) |> return;
        }
      }
    };

  | FailedCast(d1, ty, ty') =>
    let* r1 = require(d1);
    switch (r1) {
    | Expr(d1') => Expr(FailedCast(d1', ty, ty')) |> return
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty')) |> return
    };

  | InvalidOperation(d, err) => Indet(InvalidOperation(d, err)) |> return
  };
};

let rec transition_rule =
        (
          tenv: environment,
          scrut: DHExp.t,
          rules: list(DHExp.rule),
          current_rule_index: int,
        )
        : EvaluatorMonad.t(StepperResult.t(DHExp.t)) => {
  let {env, fenv, continue, _} = tenv;
  switch (List.nth_opt(rules, current_rule_index)) {
  | None =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    Indet(Closure(env, fenv, ConsistentCase(case))) |> return;
  | Some(Rule(dp, d)) =>
    switch (Evaluator.matches(dp, scrut)) {
    | IndetMatch =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      Indet(Closure(env, fenv, ConsistentCase(case))) |> return;
    | Matches(env') =>
      // extend environment with new bindings introduced
      let* env = env |> Evaluator.evaluate_extend_env(env');
      continue(~env, d);
    // by the rule and evaluate the expression.
    | DoesNotMatch =>
      transition_rule(tenv, scrut, rules, current_rule_index + 1)
    }
  };
};

let rec evaluate =
        (
          ~env: ClosureEnvironment.t,
          ~fenv: FilterEnvironment.t,
          ~fact: (FilterEnvironment.t, DHExp.t) => FilterAction.t,
          d: DHExp.t,
        ) => {
  let act = fact(fenv, d);
  let evaluate = (~env=env, ~fenv=fenv, d: DHExp.t) => {
    evaluate(~env, ~fenv, ~fact, d);
  };
  switch (act) {
  | Pause => Expr(d) |> return
  | Eval =>
    let builtin = (~env=env, ~fenv=fenv, d: DHExp.t) => {
      env |> ignore;
      fenv |> ignore;
      BoxedValue(d) |> return;
    };
    let env = {
      env,
      fenv,
      require: evaluate,
      evaluate,
      builtin,
      continue: evaluate,
    };
    transition(~env, d);
  };
};
