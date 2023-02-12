open Sexplib.Std;
open Util;
open EvaluatorMonad;
open EvaluatorMonad.Syntax;

module EvalCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Mark
    | Closure
    | Let
    | Ap1
    | Ap2
    | BinBoolOp1
    | BinBoolOp2
    | BinIntOp1
    | BinIntOp2
    | BinFloatOp1
    | BinFloatOp2
    | Tuple(int)
    | Cons1
    | Cons2
    | Inj
    | NonEmptyHole
    | Cast
    | FailedCast
    | InvalidOperation
    | ConsistentCase
    | InconsistentBranches;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Mark
    | Closure(ClosureEnvironment.t, t)
    | Let(DHPat.t, t, DHExp.t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Inj(Typ.t, InjSide.t, t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | ConsistentCase(case)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
  and case =
    | Case(t, list(rule), int)
  and rule = DHExp.rule;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t)
  | Step(DHExp.t);

let unbox =
  fun
  | Step(d)
  | BoxedValue(d)
  | Indet(d) => d;

let fast_equal = (r1, r2) =>
  switch (r1, r2) {
  | (Step(d1), Step(d2))
  | (BoxedValue(d1), BoxedValue(d2))
  | (Indet(d1), Indet(d2)) => DHExp.fast_equal(d1, d2)
  | _ => false
  };

let t_of_evaluator_result = (r: EvaluatorResult.t): t =>
  switch (r) {
  | BoxedValue(d) => BoxedValue(d)
  | Indet(d) => Indet(d)
  };

let evaluator_result_of_t = (r: t): EvaluatorResult.t =>
  switch (r) {
  | Step(d)
  | BoxedValue(d) => BoxedValue(d)
  | Indet(d) => Indet(d)
  };

/**
  Alias for EvaluatorMonad.
 */
type m('a) = EvaluatorMonad.t('a);

let matches = Evaluator.matches;

let evaluate_extend_env = Evaluator.evaluate_extend_env;

let ground_cases_of = Evaluator.ground_cases_of;

let eval_bin_bool_op = Evaluator.eval_bin_bool_op;

let eval_bin_bool_op_short_circuit = Evaluator.eval_bin_bool_op_short_circuit;

let eval_bin_int_op = Evaluator.eval_bin_int_op;

let eval_bin_float_op = Evaluator.eval_bin_float_op;

let eval_bin_string_op = Evaluator.eval_bin_string_op;

let evaluate_ap_builtin = Evaluator.evaluate_ap_builtin;

let rec transition = (env: ClosureEnvironment.t, d: DHExp.t): m(t) => {
  /* TODO: Investigate */
  /* Increment number of evaluation steps (calls to `evaluate`). */
  /* let* () = take_step; */
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
    /* This is unreachable since every bound variable should be replaced
     * in parent structure */
    // transition(env, d);
    Step(Closure(env, d)) |> return;

  | Sequence(d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(_d1)
    | BoxedValue(_d1) => transition(env, d2)
    /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
     * if evaluated d1 is indet. */
    | Indet(_d1) =>
      /* let* r2 = step(env, d2, opt); */
      /* switch (r2) { */
      /* | BoxedValue(d2) */
      /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
      /* }; */
      transition(env, d2)
    };

  | Let(dp, d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(Let(dp, d1', d2)) |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      switch (matches(dp, d1')) {
      | IndetMatch
      | DoesNotMatch => Indet(Closure(env, Let(dp, d1', d2))) |> return
      | Matches(env') =>
        let* env = evaluate_extend_env(env', env);
        Step(Closure(env, d2)) |> return;
      }
    };

  | FixF(f, _, d') =>
    let* env' = evaluate_extend_env(Environment.singleton((f, d)), env);
    transition(env', d');

  | Fun(_) => Step(Closure(env, d)) |> return

  | Ap(d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(Ap(d1', d2)) |> return
    | BoxedValue(TestLit(id)) => evaluate_test(env, id, d2)
    | BoxedValue(Tag(_)) =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2) => Step(Ap(d1, d2)) |> return
      | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
      | Indet(d2) => Indet(Ap(d1, d2)) |> return
      };
    | BoxedValue(Closure(closure_env, Fun(dp, _, d3, _)) as d1) =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(_)
      | Indet(_) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch
        | IndetMatch => Indet(Ap(d1, d2)) |> return
        // opt.pause_subexpression ? Pause(Ap(d1, d2')) : Indet(d)
        | Matches(env') =>
          // evaluate a closure: extend the closure environment with the
          // new bindings introduced by the function application.
          let* env = evaluate_extend_env(env', closure_env);
          transition(env, d3);
        }
      };
    | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        transition(env, Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFun");
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2')) |> return
      };
    };

  | ApBuiltin(ident, args) =>
    let* r = evaluate_ap_builtin(env, ident, args);
    switch (r) {
    | BoxedValue(d) => Step(d) |> return
    | Indet(d) => Indet(d) |> return
    };

  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Tag(_) => BoxedValue(d) |> return

  | BinBoolOp(op, d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(BinBoolOp(op, d1', d2)) |> return
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => Step(b3) |> return
      | None =>
        let* r2 = transition(env, d2);
        switch (r2) {
        | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
        | BoxedValue(BoolLit(b2)) =>
          Step(eval_bin_bool_op(op, b1, b2)) |> return
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
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
      };
    };

  | BinIntOp(op, d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(Closure(env, BinIntOp(op, d1', d2))) |> return
    | BoxedValue(IntLit(n1) as d1') =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(Closure(env, BinIntOp(op, d1, d2'))) |> return
      | BoxedValue(IntLit(n2)) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Step(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              DivideByZero,
            ),
          )
          |> return
        | _ => Step(eval_bin_int_op(op, n1, n2)) |> return
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
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    };

  | BinFloatOp(op, d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(BinFloatOp(op, d1', d2)) |> return
    | BoxedValue(FloatLit(f1) as d1') =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(FloatLit(f2)) =>
        Step(eval_bin_float_op(op, f1, f2)) |> return
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')));
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFloatLit");
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    };

  | BinStringOp(op, d1, d2) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(BinStringOp(op, d1', d2)) |> return
    | BoxedValue(StringLit(s1) as d1') =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(StringLit(s2)) =>
        Step(eval_bin_string_op(op, s1, s2)) |> return
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')));
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedStringLit");
      raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    };

  | Inj(ty, side, d1) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(Inj(ty, side, d1')) |> return
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1')) |> return
    | Indet(d1') => Indet(Inj(ty, side, d1')) |> return
    };

  | Tuple(ds) =>
    let+ drs =
      ds |> List.map(d => transition(env, d) >>| (r => (d, r))) |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Step(_), _) => (Step(empty), [el, ...dst])
          | (_, Step(el')) => (Step(empty), [el', ...dst])
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
    | Step(_) => Step(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | Prj(targ, n) =>
    // TODO:
    if (n < 0) {
      return(
        Indet(InvalidOperation(d, InvalidOperationError.InvalidProjection)),
      );
    } else {
      let* r = transition(env, targ);
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
          transition(env, Cast(Prj(targ', n), ty, ty'));
        }
      | _ => return(Indet(d))
      };
    }
  | Cons(d1, d2) =>
    let* r1 = transition(env, d1);
    let* r2 = transition(env, d2);
    switch (r1, r2) {
    | (Step(d1'), _) => Step(Cons(d1', d2)) |> return
    | (_, Step(d2')) => Step(Cons(d1, d2')) |> return
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

  | ListLit(u, i, err, ty, ds) =>
    let+ drs =
      ds |> List.map(d => transition(env, d) >>| (r => (d, r))) |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Step(_), _) => (Step(empty), [el, ...dst])
          | (_, Step(el')) => (Step(empty), [el', ...dst])
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

    let d' = DHExp.ListLit(u, i, err, ty, ds');

    switch (tag) {
    | Step(_) => Step(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case(env, None, d1, rules, n)

  /* Generalized closures evaluate to themselves. Only
     lambda closures are BoxedValues; other closures are all Indet. */
  | Closure(env', d') =>
    switch (d') {
    | Fun(_) => BoxedValue(d) |> return
    | _ =>
      /* We merge the outside env, and closure env here to avoid closure
         inside closure. */
      let* env = ClosureEnvironment.union(env', env) |> with_eig;
      let* r = transition(env, d');
      switch (r) {
      | Step(d) => Step(Closure(env, d)) |> return
      | BoxedValue(d) =>
        /* If [d'] evaluates to [BoxedValue], then either [d] no longer
         * contains any [BoundVar], or [d] has already contains a
         * closure, like [Fun]. */
        BoxedValue(d) |> return
      | Indet(d) => Indet(d) |> return
      };
    }

  /* Hole expressions */
  | InconsistentBranches(u, i, Case(d1, rules, n)) =>
    evaluate_case(env, Some((u, i)), d1, rules, n)

  | EmptyHole(u, i) => Indet(Closure(env, EmptyHole(u, i))) |> return

  | NonEmptyHole(reason, u, i, d1) =>
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') =>
      Step(Closure(env, NonEmptyHole(reason, u, i, d1'))) |> return
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
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(Cast(d1', ty, ty')) |> return
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
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(env, d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(env, d');
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
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(env, d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(env, d');
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
    let* r1 = transition(env, d1);
    switch (r1) {
    | Step(d1') => Step(FailedCast(d1', ty, ty')) |> return
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty')) |> return
    };

  | InvalidOperation(d, err) => Indet(InvalidOperation(d, err)) |> return
  };
}

and evaluate_case =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : m(t) => {
  let* rscrut = transition(env, scrut);
  switch (rscrut) {
  | Step(scrut) =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    (
      switch (inconsistent_info) {
      | None => Step(Closure(env, ConsistentCase(case)))
      | Some((u, i)) =>
        Step(Closure(env, InconsistentBranches(u, i, case)))
      }
    )
    |> return;
  | BoxedValue(scrut)
  | Indet(scrut) =>
    eval_rule(env, inconsistent_info, scrut, rules, current_rule_index)
  };
}

and eval_rule =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : m(t) => {
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
      Step(Closure(env, d)) |> return;
    // by the rule and evaluate the expression.
    | DoesNotMatch =>
      eval_rule(env, inconsistent_info, scrut, rules, current_rule_index + 1)
    }
  };
}

and evaluate_test =
    (env: ClosureEnvironment.t, n: KeywordID.t, arg: DHExp.t): m(t) => {
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
      let* arg_d1 = transition(env, arg_d1);
      let* arg_d2 = transition(env, arg_d2);
      let* arg_d3 = transition(env, arg_d3);
      let arg_show =
        DHExp.Ap(Ap(unbox(arg_d1), unbox(arg_d2)), unbox(arg_d3));
      let* arg_result = transition(env, arg_show);
      (arg_show, arg_result) |> return;

    | Ap(arg_d1, arg_d2) =>
      let mk = (arg_d1, arg_d2) => DHExp.Ap(arg_d1, arg_d2);
      evaluate_test_eq(env, mk, arg_d1, arg_d2);

    | _ =>
      let* arg = transition(env, arg);
      (unbox(arg), arg) |> return;
    };

  let test_status: TestStatus.t =
    switch (arg_result) {
    | BoxedValue(BoolLit(true)) => Pass
    | BoxedValue(BoolLit(false)) => Fail
    | _ => Indet
    };

  let* _ = add_test(n, (arg_show, test_status));
  let r: t =
    switch (arg_result) {
    | BoxedValue(BoolLit(_)) => BoxedValue(Tuple([]))
    // TODO:
    | Step(arg)
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
    : m((DHExp.t, t)) => {
  let* arg_d1 = transition(env, arg_d1);
  let* arg_d2 = transition(env, arg_d2);

  let arg_show = mk_arg_op(unbox(arg_d1), unbox(arg_d2));
  let* arg_result = transition(env, arg_show);

  (arg_show, arg_result) |> return;
};

// Fig.11 Final Forms
// d final
let is_final = (d: t): bool =>
  switch (d) {
  | Step(_) => false
  | BoxedValue(_)
  | Indet(_) => true
  };

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t,
    ctx: EvalCtx.t,
    exp: DHExp.t,
  };

  let mk = (env: ClosureEnvironment.t, ctx: EvalCtx.t, exp: DHExp.t): t => {
    env,
    ctx,
    exp,
  };

  let init = (exp: DHExp.t): t => {
    let es = EvaluatorState.init;
    let env = Environment.empty;
    let fenv =
      env |> ClosureEnvironment.of_environment |> EvaluatorState.with_eig;
    let (env, _) = es |> fenv;
    {env, ctx: Mark, exp};
  };

  let get_env = (obj: t): ClosureEnvironment.t => obj.env;
  let get_ctx = (obj: t): EvalCtx.t => obj.ctx;
  let get_exp = (obj: t): DHExp.t => obj.exp;

  let unwrap = (obj: t, sel: EvalCtx.cls): option(t) => {
    switch (sel, obj.ctx) {
    | (Mark, _) =>
      print_endline(
        "Mark does not match with "
        ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(obj.ctx)),
      );
      raise(EvaluatorError.Exception(StepDoesNotMatch));
    | (NonEmptyHole, _)
    | (InconsistentBranches, _) =>
      raise(EvaluatorPost.Exception(PostprocessedHoleOutsideClosure))
    | (Closure, Closure(_, c))
    | (Let, Let(_, c, _))
    | (Ap1, Ap1(c, _))
    | (Ap2, Ap2(_, c))
    | (BinBoolOp1, BinBoolOp1(_, c, _))
    | (BinBoolOp2, BinBoolOp2(_, _, c))
    | (BinIntOp1, BinIntOp1(_, c, _))
    | (BinIntOp2, BinIntOp2(_, _, c))
    | (BinFloatOp1, BinFloatOp1(_, c, _))
    | (BinFloatOp2, BinFloatOp2(_, _, c))
    | (Cons1, Cons1(c, _))
    | (Cons2, Cons2(_, c))
    | (Inj, Inj(_, _, c)) => Some({...obj, ctx: c})
    | (Tuple(n), Tuple(c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (ConsistentCase, ConsistentCase(Case(scrut, _, _))) =>
      Some({...obj, ctx: scrut})
    | (Cast, Cast(c, _, _)) => Some({...obj, ctx: c})
    | (Ap1, Ap2(_, _))
    | (Ap2, Ap1(_, _))
    | (BinBoolOp1, BinBoolOp2(_))
    | (BinBoolOp2, BinBoolOp1(_))
    | (BinIntOp1, BinIntOp2(_))
    | (BinIntOp2, BinIntOp1(_))
    | (BinFloatOp1, BinFloatOp2(_))
    | (BinFloatOp2, BinFloatOp1(_))
    | (Cons1, Cons2(_))
    | (Cons2, Cons1(_)) => None
    | (tag, ctx) =>
      print_endline(
        Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_cls(tag))
        ++ " does not match with "
        ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(ctx)),
      );
      raise(EvaluatorError.Exception(StepDoesNotMatch));
    };
  };
};

let rec decompose =
        (env: ClosureEnvironment.t, d: DHExp.t): m(list(EvalObj.t)) => {
  let wrap = (fctx: EvalCtx.t => EvalCtx.t, ld: list(EvalObj.t)) =>
    List.map((obj: EvalObj.t) => {...obj, ctx: fctx(obj.ctx)}, ld);

  let* r = transition(env, d);
  if (is_final(r)) {
    [] |> return;
  } else {
    switch (d) {
    | Closure(env', d1) =>
      let* env = env |> ClosureEnvironment.union(env') |> with_eig;
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d1)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => Closure(env, c), ld1) |> return;
      };
    | Sequence(_)
    | ApBuiltin(_)
    | TestLit(_)
    | StringLit(_)
    | BinStringOp(_)
    | Prj(_)
    | Tag(_)
    | FreeVar(_)
    | InvalidText(_)
    | Fun(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | ListLit(_, _, _, _, _)
    | EmptyHole(_)
    | FixF(_, _, _)
    | BoundVar(_)
    | ExpandingKeyword(_) => [EvalObj.mk(env, Mark, d)] |> return
    | Ap(d1, d2) =>
      let* r1 = transition(env, d1);
      let* r2 = transition(env, d2);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        let* ld2 = decompose(env, d2);
        wrap(c => Ap1(c, d2), ld1) @ wrap(c => Ap2(d1, c), ld2) |> return;
      };
    | NonEmptyHole(reason, u, i, d1) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => NonEmptyHole(reason, u, i, c), ld1) |> return;
      };
    | BinBoolOp(op, d1, d2) =>
      let* r1 = transition(env, d1);
      let* r2 = transition(env, d2);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        let* ld2 = decompose(env, d2);
        wrap(c => BinBoolOp1(op, c, d2), ld1)
        @ wrap(c => BinBoolOp2(op, d1, c), ld2)
        |> return;
      };
    | BinIntOp(op, d1, d2) =>
      let* r1 = transition(env, d1);
      let* r2 = transition(env, d2);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        let* ld2 = decompose(env, d2);
        wrap(c => BinIntOp1(op, c, d2), ld1)
        @ wrap(c => BinIntOp2(op, d1, c), ld2)
        |> return;
      };
    | BinFloatOp(op, d1, d2) =>
      let* r1 = transition(env, d1);
      let* r2 = transition(env, d2);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        let* ld2 = decompose(env, d2);
        wrap(c => BinFloatOp1(op, c, d2), ld1)
        @ wrap(c => BinFloatOp2(op, d1, c), ld2)
        |> return;
      };
    | Cons(d1, d2) =>
      let* r1 = transition(env, d1);
      let* r2 = transition(env, d2);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        let* ld2 = decompose(env, d2);
        wrap(c => Cons1(c, d2), ld1)
        @ wrap(c => Cons2(d1, c), ld2)
        |> return;
      };
    | Cast(d1, ty1, ty2) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => Cast(c, ty1, ty2), ld1) |> return;
      };
    | FailedCast(d1, ty1, ty2) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => FailedCast(c, ty1, ty2), ld1) |> return;
      };
    | Tuple(ld) =>
      let* is_final = {
        let f = (pr, d) => {
          let* r = transition(env, d);
          let* pr = pr;
          (pr && is_final(r)) |> return;
        };
        ld |> List.fold_left(f, false |> return);
      };
      if (is_final) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let rec walk = (ld, rd, rc) =>
          switch (rd) {
          | [] => rc |> return
          | [hd, ...tl] =>
            let* c = decompose(env, hd);
            let f = (obj: EvalObj.t) =>
              EvalObj.mk(env, Tuple(obj.ctx, (ld, tl)), obj.exp);
            walk(ld @ [hd], tl, rc @ (c |> List.map(f)));
          };
        let* ret = walk([], ld, []);
        ret |> return;
      };
    | Let(dp, d1, d2) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => Let(dp, c, d2), ld1) |> return;
      };
    | Inj(ty, side, d1) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => Inj(ty, side, c), ld1) |> return;
      };
    | InvalidOperation(d1, err) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => InvalidOperation(c, err), ld1) |> return;
      };
    | ConsistentCase(Case(d1, rule, n)) =>
      print_endline("decomposing ConsistentCase...");
      let* r1 = transition(env, d1);
      print_endline("r1: " ++ Sexplib.Sexp.to_string_hum(sexp_of_t(r1)));
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => ConsistentCase(Case(c, rule, n)), ld1) |> return;
      };
    | InconsistentBranches(u, i, Case(d1, rule, n)) =>
      let* r1 = transition(env, d1);
      if (is_final(r1)) {
        [EvalObj.mk(env, Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1);
        wrap(c => InconsistentBranches(u, i, Case(c, rule, n)), ld1)
        |> return;
      };
    };
  };
};

let rec compose = (ctx: EvalCtx.t, d: DHExp.t): DHExp.t => {
  switch (ctx) {
  | Mark => d
  | Closure(env, ctx) => Closure(env, compose(ctx, d))
  | Ap1(ctx1, d1) => Ap(compose(ctx1, d), d1)
  | Ap2(d1, ctx1) => Ap(d1, compose(ctx1, d))
  | BinBoolOp1(op, ctx1, d1) => BinBoolOp(op, compose(ctx1, d), d1)
  | BinBoolOp2(op, d1, ctx1) => BinBoolOp(op, d1, compose(ctx1, d))
  | BinIntOp1(op, ctx1, d1) => BinIntOp(op, compose(ctx1, d), d1)
  | BinIntOp2(op, d1, ctx1) => BinIntOp(op, d1, compose(ctx1, d))
  | BinFloatOp1(op, ctx1, d1) => BinFloatOp(op, compose(ctx1, d), d1)
  | BinFloatOp2(op, d1, ctx1) => BinFloatOp(op, d1, compose(ctx1, d))
  | Cons1(ctx1, d1) => Cons(compose(ctx1, d), d1)
  | Cons2(d1, ctx1) => Cons(d1, compose(ctx1, d))
  | Tuple(ctx, (ld, rd)) => Tuple(ld @ [compose(ctx, d), ...rd])
  | Let(dp, ctx1, d1) => Let(dp, compose(ctx1, d), d1)
  | Inj(ty, side, ctx1) => Inj(ty, side, compose(ctx1, d))
  | Cast(ctx1, ty1, ty2) => Cast(compose(ctx1, d), ty1, ty2)
  | FailedCast(ctx1, ty1, ty2) => FailedCast(compose(ctx1, d), ty1, ty2)
  | InvalidOperation(ctx1, err) => InvalidOperation(compose(ctx1, d), err)
  | NonEmptyHole(reason, u, i, ctx1) =>
    NonEmptyHole(reason, u, i, compose(ctx1, d))
  | ConsistentCase(Case(ctx1, rule, n)) =>
    ConsistentCase(Case(compose(ctx1, d), rule, n))
  | InconsistentBranches(u, i, Case(ctx1, rule, n)) =>
    InconsistentBranches(u, i, Case(compose(ctx1, d), rule, n))
  };
};

let compose = (ctx: EvalCtx.t, d: DHExp.t): DHExp.t => {
  print_endline(
    "composing: ctx: " ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(ctx)),
  );
  print_endline(
    "composing: d: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  );
  let d = compose(ctx, d);
  print_endline(
    "composed: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  );
  d;
};

let transition = (env: ClosureEnvironment.t, d: DHExp.t): m(t) => {
  print_endline(
    "transitioning: env: "
    ++ Sexplib.Sexp.to_string_hum(ClosureEnvironment.sexp_of_t(env)),
  );
  print_endline(
    "transitioning: d: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  );
  let+ r = transition(env, d);
  print_endline(
    "transitioned: " ++ Sexplib.Sexp.to_string_hum(sexp_of_t(r)),
  );
  r;
};

let step = (env: ClosureEnvironment.t, obj: EvalObj.t): m(t) => {
  let* env = ClosureEnvironment.union(obj.env, env) |> with_eig;
  let* r = transition(env, obj.exp);
  let d = compose(obj.ctx, unbox(r));
  switch (r) {
  | Step(_) => Step(d) |> return
  | BoxedValue(_) => BoxedValue(d) |> return
  | Indet(_) => Indet(d) |> return
  };
};

let step = (env: Environment.t, obj: EvalObj.t) => {
  let es = EvaluatorState.init;
  let (env, es) =
    es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env));
  step(env, obj, es);
};

let decompose = (env: Environment.t, d: DHExp.t) => {
  print_endline(
    "decomposing: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  );
  let es = EvaluatorState.init;
  let (env, es) =
    es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env));
  let (es, ld) = decompose(env, d, es);
  print_endline(
    "decomposed: "
    ++ Sexplib.Sexp.to_string_hum(
         Sexplib.Std.sexp_of_list(EvalObj.sexp_of_t, ld),
       ),
  );
  (es, ld);
};
