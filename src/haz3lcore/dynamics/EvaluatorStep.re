open Sexplib.Std;
open Util;
open EvaluatorMonad;
open EvaluatorMonad.Syntax;

module EvalCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Mark
    | Closure
    | Filter
    | Sequence
    | Let
    | Ap1
    | Ap2
    | BinBoolOp1
    | BinBoolOp2
    | BinIntOp1
    | BinIntOp2
    | BinFloatOp1
    | BinFloatOp2
    | BinStringOp1
    | BinStringOp2
    | Tuple(int)
    | ListLit(int)
    | Cons1
    | Cons2
    | Prj
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
    | Filter(FilterAction.t, t)
    | Sequence(t, DHExp.t)
    | Let(DHPat.t, t, DHExp.t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | BinStringOp1(DHExp.BinStringOp.t, t, DHExp.t)
    | BinStringOp2(DHExp.BinStringOp.t, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | ListLit(
        MetaVar.t,
        MetaVarInst.t,
        ListErrStatus.t,
        Typ.t,
        t,
        (list(DHExp.t), list(DHExp.t)),
      )
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Prj(t, int)
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
type cls =
  | BoxedValue
  | Indet
  | Step;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t)
  | Step(DHExp.t);

let unbox: t => DHExp.t =
  fun
  | Step(d)
  | BoxedValue(d)
  | Indet(d) => d;

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

let rec transition =
        (
          ~env: ClosureEnvironment.t,
          ~fact: FilterAction.t,
          ~pause: bool,
          d: DHExp.t,
        )
        : m(t) => {
  let transition = (~env=env, ~fact=fact, ~pause=pause, d) => {
    transition(~env, ~fact, ~pause, d);
  };
  print_endline(
    "transition: dexp = " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  );
  print_endline(
    "transition: fact = "
    ++ Sexplib.Sexp.to_string_hum(FilterAction.sexp_of_t(fact)),
  );
  let fact: FilterAction.t =
    switch (pause, fact) {
    | (true, Step) => Keep
    | _ => fact
    };
  let fact_decr = fact |> FilterAction.decr;
  /* TODO: Investigate */
  /* Increment number of evaluation steps (calls to `evaluate`). */
  let* () = take_step;
  switch (d) {
  | BoundVar(x) =>
    let d =
      x
      |> ClosureEnvironment.lookup(env)
      |> OptUtil.get(() => {
           print_endline("FreeInvalidVar: " ++ x);
           raise(EvaluatorError.Exception(FreeInvalidVar(x)));
         });
    /* We need to call [evaluate] on [d] again since [env] does not store
     * final expressions. */
    switch (fact) {
    | Keep => Step(BoundVar(x)) |> return
    | Step => Step(d) |> return
    | Eval => transition(~fact=fact_decr, d)
    };

  | Sequence(d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') => Step(Sequence(d1', d2)) |> return
    | BoxedValue(d1')
    /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
     * if evaluated d1 is indet. */
    | Indet(d1') =>
      /* let* r2 = step(env, d2, opt); */
      /* switch (r2) { */
      /* | BoxedValue(d2) */
      /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
      /* }; */
      switch (fact) {
      | Keep => Step(Sequence(d1', d2)) |> return
      | Step => Step(d2) |> return
      | Eval => transition(~fact=fact_decr, d2)
      }
    };

  | Let(dp, d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') => Step(Let(dp, d1', d2)) |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      switch (matches(dp, d1')) {
      | IndetMatch
      | DoesNotMatch => Indet(Let(dp, d1', d2)) |> return
      | Matches(env') =>
        let* env = evaluate_extend_env(env', env);
        switch (fact) {
        | Keep => Step(Let(dp, d1', d2)) |> return
        | Step => Step(Closure(env, d2)) |> return
        | Eval =>
          let* r2 = transition(~env, ~fact=fact_decr, d2);
          switch (r2) {
          | Step(d2') => Step(Closure(env, d2')) |> return
          | BoxedValue(_)
          | Indet(_) => r2 |> return
          };
        };
      }
    };

  | FixF(name, ty, d1) =>
    let* env' = evaluate_extend_env(Environment.singleton((name, d)), env);
    switch (fact) {
    | Keep => Step(FixF(name, ty, d1)) |> return
    | Step => Step(Closure(env', d1)) |> return
    | Eval =>
      let* r1 = transition(~env=env', ~fact=fact_decr, d1);
      switch (r1) {
      | Step(d1') => Step(Closure(env', d1')) |> return
      | BoxedValue(_)
      | Indet(_) => r1 |> return
      };
    };

  | Fun(_) =>
    switch (fact) {
    | Keep => Step(d) |> return
    | Step => Step(Closure(env, Filter(fact, d))) |> return
    | Eval => BoxedValue(Closure(env, Filter(fact, d))) |> return
    }

  | Ap(d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') =>
      let* r2 = transition(~fact=fact_decr, d2);
      switch (r2) {
      | Step(d2')
      | BoxedValue(d2')
      | Indet(d2') => Step(Ap(d1', d2')) |> return
      };
    | BoxedValue(TestLit(id)) => evaluate_test(~env, ~fact, ~pause, id, d2)
    | BoxedValue(Tag(_)) =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2') => BoxedValue(Ap(d1, d2')) |> return
      | Indet(d2') => Indet(Ap(d1, d2')) |> return
      };
    | BoxedValue(
        Closure(closure_env, Filter(closure_fact, Fun(dp, _, d3, _))) as d1',
      ) =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1', d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') =>
        switch (matches(dp, d2')) {
        | DoesNotMatch
        | IndetMatch => Indet(Ap(d1', d2')) |> return
        | Matches(env') =>
          /* evaluate a closure: extend the closure environment with the
           * new bindings introduced by the function application. */
          let* env = evaluate_extend_env(env', closure_env);
          switch (fact) {
          | Keep => Step(Ap(d1', d2')) |> return
          | Step => Step(Closure(env, Filter(closure_fact, d3))) |> return
          | Eval =>
            let* r3 = transition(~env, ~fact=closure_fact, d3);
            switch (r3) {
            | Step(d3') =>
              Step(Closure(env, Filter(closure_fact, d3'))) |> return
            | BoxedValue(_)
            | Indet(_) => r3 |> return
            };
          };
        }
      };
    | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        let d' = DHExp.Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2');
        switch (fact) {
        | Keep => Step(d) |> return
        | Step => Step(d') |> return
        | Eval => transition(d')
        };
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFun");
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
    | Indet(d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2')) |> return
      };
    };

  | ApBuiltin(ident, args) =>
    let* r = evaluate_ap_builtin(env, ident, args);
    switch (fact, r) {
    | (Keep, _) => Step(ApBuiltin(ident, args)) |> return
    | (Step, BoxedValue(d)) => Step(d) |> return
    | (_, BoxedValue(d)) => BoxedValue(d) |> return
    | (_, Indet(d)) => Indet(d) |> return
    };

  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Tag(_) => BoxedValue(d) |> return

  | BinBoolOp(op, d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') => Step(BinBoolOp(op, d1', d2)) |> return
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => Step(b3) |> return
      | None =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
        | BoxedValue(BoolLit(b2) as d2') =>
          switch (fact) {
          | Keep => Step(BinBoolOp(op, d1', d2')) |> return
          | Step => Step(eval_bin_bool_op(op, b1, b2)) |> return
          | Eval => BoxedValue(eval_bin_bool_op(op, b1, b2)) |> return
          }
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
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinBoolOp(op, d1', d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
      };
    };

  | BinIntOp(op, d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') =>
      let* r2 = transition(~fact=fact_decr, d2);
      switch (r2) {
      | Step(d2')
      | BoxedValue(d2')
      | Indet(d2') => Step(BinIntOp(op, d1', d2')) |> return
      };
    | BoxedValue(IntLit(n1) as d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1', d2')) |> return
      | BoxedValue(IntLit(n2) as d2') =>
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
        | _ =>
          switch (fact) {
          | Keep => Step(BinIntOp(op, d1', d2')) |> return
          | Step => Step(eval_bin_int_op(op, n1, n2)) |> return
          | Eval => BoxedValue(eval_bin_int_op(op, n1, n2)) |> return
          }
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedIntLit1");
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')));
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedIntLit2");
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1', d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    };

  | BinFloatOp(op, d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') =>
      let* r2 = transition(~fact=fact_decr, d2);
      switch (r2) {
      | Step(d2')
      | BoxedValue(d2')
      | Indet(d2') => Step(BinFloatOp(op, d1', d2')) |> return
      };
    | BoxedValue(FloatLit(f1) as d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(FloatLit(f2) as d2') =>
        switch (fact) {
        | Keep => Step(BinFloatOp(op, d1', d2')) |> return
        | Step => Step(eval_bin_float_op(op, f1, f2)) |> return
        | Eval => BoxedValue(eval_bin_float_op(op, f1, f2)) |> return
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')));
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFloatLit");
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1', d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    };

  | BinStringOp(op, d1, d2) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') =>
      let* r2 = transition(~fact=fact_decr, d2);
      switch (r2) {
      | Step(d2')
      | BoxedValue(d2')
      | Indet(d2') => Step(BinStringOp(op, d1', d2')) |> return
      };
    | BoxedValue(StringLit(s1) as d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(StringLit(s2) as d2') =>
        switch (fact) {
        | Keep => Step(BinStringOp(op, d1', d2')) |> return
        | Step => Step(eval_bin_string_op(op, s1, s2)) |> return
        | Eval => BoxedValue(eval_bin_string_op(op, s1, s2)) |> return
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')));
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedStringLit");
      raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(d2);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1', d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    };

  | Inj(ty, side, d1) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') => Step(Inj(ty, side, d1')) |> return
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1')) |> return
    | Indet(d1') => Indet(Inj(ty, side, d1')) |> return
    };

  | Tuple(ds) =>
    let+ rs =
      ds |> List.map(d => transition(d) >>| (r => (d, r))) |> sequence;
    // let rec eval_tuple = (cls: cls, rs: list(DHExp.t), ds) =>
    //   switch (ds) {
    //   | [] =>
    //     switch (cls) {
    //     | Step => Step(Tuple(rs)) |> return;
    //     | BoxedValue => BoxedValue(Tuple(rs)) |> return;
    //     | Indet => Indet(Tuple(rs)) |> return;
    //     };
    //   | [hd, ...tl] =>
    //     switch (cls, rs) {
    //     | (Step, rs') =>
    //       let* hr = transition(~pause=false, hd);
    //       switch (hr) {
    //       | Step(hd')
    //       | BoxedValue(hd')
    //       | Indet(hd') => eval_tuple(Step, [hd', ...rs'], tl);
    //       };
    //     | (Indet, rs') =>
    //       let* r = transition(hd);
    //       switch (r) {
    //       | Step(hd') => eval_tuple(Step, [hd', ...rs'], tl);
    //       | BoxedValue(hd')
    //       | Indet(hd') => eval_tuple(Indet, [hd', ...rs'], tl);
    //       };
    //     | (BoxedValue, rs') =>
    //       let* r = transition(hd);
    //       switch (r) {
    //       | Step(hd') => eval_tuple(Step, [hd', ...rs'], tl);
    //       | Indet(hd') => eval_tuple(Indet, [hd', ...rs'], tl);
    //       | BoxedValue(hd') => eval_tuple(BoxedValue, [hd', ...rs'], tl);
    //       };
    //     };
    //   };

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
        rs,
        (BoxedValue(empty), []),
      );

    let d' = DHExp.Tuple(ds');

    switch (tag) {
    | Step(_) => Step(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | Prj(targ, n) =>
    if (n < 0) {
      return(
        Indet(InvalidOperation(d, InvalidOperationError.InvalidProjection)),
      );
    } else {
      let* r = transition(targ);
      switch (r) {
      | Step(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          Indet(
            InvalidOperation(rv, InvalidOperationError.InvalidProjection),
          )
          |> return;
        } else {
          Step(List.nth(ds, n)) |> return;
        }
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
      | Indet(Closure(_, Filter(_, Tuple(ds))) as rv)
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
      | Step(Cast(targ', Prod(tys), Prod(tys')) as rv)
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
          Step(Closure(env, Filter(fact, Cast(Prj(targ', n), ty, ty'))))
          |> return;
        }
      | Step(d) => Step(Prj(d, n)) |> return
      | _ => return(Indet(d))
      };
    }

  | Cons(d1, d2) =>
    let* r1 = transition(d1);
    let* r2 = transition(d2);
    switch (r1, r2) {
    | (Step(d1'), _) => Step(Cons(d1', d2)) |> return
    | (_, Step(d2')) => Step(Cons(d1, d2')) |> return
    | (Indet(d1'), Indet(d2'))
    | (Indet(d1'), BoxedValue(d2'))
    | (BoxedValue(d1'), Indet(d2')) => Indet(Cons(d1', d2')) |> return
    | (BoxedValue(d1'), BoxedValue(d2')) =>
      switch (d2') {
      | ListLit(u, i, err, ty, ds) =>
        BoxedValue(ListLit(u, i, err, ty, [d1', ...ds])) |> return
      | Cons(_)
      | Cast(ListLit(_), List(_), List(_)) =>
        BoxedValue(Cons(d1', d2')) |> return
      | _ =>
        print_endline("InvalidBoxedListLit");
        raise(EvaluatorError.Exception(InvalidBoxedListLit(d2')));
      }
    };

  | ListLit(u, i, err, ty, ds) =>
    let+ drs =
      ds |> List.map(d => transition(d) >>| (r => (d, r))) |> sequence;

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
    switch (fact) {
    | Keep => Step(d) |> return
    | Step
    | Eval => evaluate_case(env, None, d1, rules, n, fact_decr, pause)
    }

  /* Generalized closures evaluate to themselves. Only
     lambda closures are BoxedValues; other closures are all Indet. */
  | Closure(env', d1) =>
    switch (d1) {
    | Filter(_, Fun(_)) => BoxedValue(d) |> return
    | Fun(_) =>
      switch (fact) {
      | Keep => Step(d) |> return
      | Step => Step(Closure(env', Filter(fact, d))) |> return
      | _ => BoxedValue(Closure(env', Filter(fact, d))) |> return
      }
    | d1 =>
      let* r1 = transition(~env=env', d1);
      switch (r1) {
      | Step(d1') => Step(Closure(env', d1')) |> return
      | BoxedValue(d1') => BoxedValue(d1') |> return
      | Indet(d1') => Indet(d1') |> return
      };
    }

  | Filter(fact', d1) =>
    let fact: FilterAction.t =
      switch (fact, fact') {
      | (Keep, Eval) => Eval
      | (Keep, _) => Keep
      | (_, fact') => fact'
      };
    let* r1 = transition(~fact, d1);
    switch (r1) {
    | Step(d1') => Step(Filter(fact', d1')) |> return
    | BoxedValue(_)
    | Indet(_) => r1 |> return
    };

  /* Hole expressions */
  | InconsistentBranches(u, i, Case(d1, rules, n)) =>
    //TODO: revisit this, consider some kind of dynamic casting
    Indet(
      Closure(
        env,
        Filter(fact, InconsistentBranches(u, i, Case(d1, rules, n))),
      ),
    )
    |> return

  | EmptyHole(u, i) =>
    Indet(Closure(env, Filter(fact, EmptyHole(u, i)))) |> return

  | NonEmptyHole(reason, u, i, d1) =>
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') =>
      Step(Closure(env, Filter(fact, NonEmptyHole(reason, u, i, d1'))))
      |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      Indet(Closure(env, Filter(fact, NonEmptyHole(reason, u, i, d1'))))
      |> return
    };

  | FreeVar(u, i, x) =>
    Indet(Closure(env, Filter(fact, FreeVar(u, i, x)))) |> return

  | ExpandingKeyword(u, i, kw) =>
    Indet(Closure(env, Filter(fact, ExpandingKeyword(u, i, kw)))) |> return

  | InvalidText(u, i, text) =>
    Indet(Closure(env, Filter(fact, InvalidText(u, i, text)))) |> return

  /* Cast calculus */
  | Cast(d1, ty, ty') =>
    let* r1 = transition(d1);
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
          print_endline("CastBVHoleGround");
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')));
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(d');
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
        transition(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(d');
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
    let* r1 = transition(d1);
    switch (r1) {
    | Step(d1') => Step(FailedCast(d1', ty, ty')) |> return
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
      fact: FilterAction.t,
      pause: bool,
    )
    : m(t) => {
  let* rscrut = transition(~env, ~fact, ~pause, scrut);
  switch (rscrut) {
  | Step(scrut) =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    switch (inconsistent_info) {
    | None =>
      Step(Closure(env, Filter(fact, ConsistentCase(case)))) |> return
    | Some((u, i)) =>
      Step(Closure(env, Filter(fact, InconsistentBranches(u, i, case))))
      |> return
    };
  | BoxedValue(scrut)
  | Indet(scrut) =>
    eval_rule(
      env,
      inconsistent_info,
      scrut,
      rules,
      current_rule_index,
      fact,
      pause,
    )
  };
}

and eval_rule =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      fact: FilterAction.t,
      pause: bool,
    )
    : m(t) => {
  switch (List.nth_opt(rules, current_rule_index)) {
  | None =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    (
      switch (inconsistent_info) {
      | None => Indet(Closure(env, Filter(fact, ConsistentCase(case))))
      | Some((u, i)) =>
        Indet(Closure(env, Filter(fact, InconsistentBranches(u, i, case))))
      }
    )
    |> return;
  | Some(Rule(dp, d)) =>
    switch (matches(dp, scrut)) {
    | IndetMatch =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      (
        switch (inconsistent_info) {
        | None => Indet(Closure(env, Filter(fact, ConsistentCase(case))))
        | Some((u, i)) =>
          Indet(
            Closure(env, Filter(fact, InconsistentBranches(u, i, case))),
          )
        }
      )
      |> return;
    | Matches(env') =>
      // extend environment with new bindings introduced
      let* env = evaluate_extend_env(env', env);
      switch (fact) {
      | Keep => Step(Closure(env, d)) |> return
      | Step => Step(Closure(env, d)) |> return
      | _ => transition(~env, ~fact, ~pause, d)
      };
    // by the rule and evaluate the expression.
    | DoesNotMatch =>
      eval_rule(
        env,
        inconsistent_info,
        scrut,
        rules,
        current_rule_index + 1,
        fact,
        pause,
      )
    }
  };
}

and evaluate_test =
    (
      ~env: ClosureEnvironment.t,
      ~fact: FilterAction.t,
      ~pause: bool,
      n: KeywordID.t,
      arg: DHExp.t,
    )
    : m(t) => {
  let* (arg_show, arg_result) =
    switch (DHExp.strip_casts(arg)) {
    | BinBoolOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinBoolOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, fact, pause);
    | BinIntOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinIntOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, fact, pause);
    | BinFloatOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinFloatOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, fact, pause);

    | Ap(Ap(arg_d1, arg_d2), arg_d3) =>
      let* arg_d1 = transition(~env, ~fact, ~pause, arg_d1);
      let* arg_d2 = transition(~env, ~fact, ~pause, arg_d2);
      let* arg_d3 = transition(~env, ~fact, ~pause, arg_d3);
      let arg_show =
        DHExp.Ap(Ap(unbox(arg_d1), unbox(arg_d2)), unbox(arg_d3));
      let* arg_result = transition(~env, ~fact, ~pause, arg_show);
      (arg_show, arg_result) |> return;

    | Ap(arg_d1, arg_d2) =>
      let mk = (arg_d1, arg_d2) => DHExp.Ap(arg_d1, arg_d2);
      evaluate_test_eq(env, mk, arg_d1, arg_d2, fact, pause);

    | _ =>
      let* arg = transition(~env, ~fact, ~pause, arg);
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
      fact: FilterAction.t,
      pause: bool,
    )
    : m((DHExp.t, t)) => {
  let* arg_d1 = transition(~env, ~fact, ~pause, arg_d1);
  let* arg_d2 = transition(~env, ~fact, ~pause, arg_d2);

  let arg_show = mk_arg_op(unbox(arg_d1), unbox(arg_d2));
  let* arg_result = transition(~env, ~fact, ~pause, arg_show);

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
    flt: FilterAction.t,
    ctx: EvalCtx.t,
    exp: DHExp.t,
  };

  let mk = (env, flt, ctx, exp) => {env, flt, ctx, exp};

  let init = (exp: DHExp.t): t => {
    let (env, _) =
      Builtins.Pervasives.builtins_as_environment
      |> ClosureEnvironment.of_environment
      |> EvaluatorState.with_eig(_, EvaluatorState.init);
    {env, ctx: Mark, exp, flt: Step};
  };

  let get_ctx = (obj: t): EvalCtx.t => obj.ctx;
  let get_exp = (obj: t): DHExp.t => obj.exp;

  let rec unwrap = (obj: t, sel: EvalCtx.cls): option(t) => {
    switch (sel, obj.ctx) {
    | (Mark, _) =>
      print_endline(
        "Mark does not match with "
        ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(obj.ctx)),
      );
      raise(EvaluatorError.Exception(StepDoesNotMatch));
    | (NonEmptyHole, NonEmptyHole(_, _, _, c))
    | (Closure, Closure(_, c))
    | (Filter, Filter(_, c))
    | (Sequence, Sequence(c, _))
    | (Let, Let(_, c, _))
    | (Ap1, Ap1(c, _))
    | (Ap2, Ap2(_, c))
    | (BinBoolOp1, BinBoolOp1(_, c, _))
    | (BinBoolOp2, BinBoolOp2(_, _, c))
    | (BinIntOp1, BinIntOp1(_, c, _))
    | (BinIntOp2, BinIntOp2(_, _, c))
    | (BinFloatOp1, BinFloatOp1(_, c, _))
    | (BinFloatOp2, BinFloatOp2(_, _, c))
    | (BinStringOp1, BinStringOp1(_, c, _))
    | (BinStringOp2, BinStringOp2(_, _, c))
    | (Cons1, Cons1(c, _))
    | (Cons2, Cons2(_, c))
    | (Prj, Prj(c, _))
    | (Inj, Inj(_, _, c)) => Some({...obj, ctx: c})
    | (Tuple(n), Tuple(c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (ListLit(n), ListLit(_, _, _, _, c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (InconsistentBranches, InconsistentBranches(_, _, Case(scrut, _, _))) =>
      Some({...obj, ctx: scrut})
    | (ConsistentCase, ConsistentCase(Case(scrut, _, _))) =>
      Some({...obj, ctx: scrut})
    | (Cast, Cast(c, _, _))
    | (FailedCast, FailedCast(c, _, _)) => Some({...obj, ctx: c})
    | (Ap1, Ap2(_, _))
    | (Ap2, Ap1(_, _))
    | (BinBoolOp1, BinBoolOp2(_))
    | (BinBoolOp2, BinBoolOp1(_))
    | (BinIntOp1, BinIntOp2(_))
    | (BinIntOp2, BinIntOp1(_))
    | (BinFloatOp1, BinFloatOp2(_))
    | (BinFloatOp2, BinFloatOp1(_))
    | (BinStringOp1, BinStringOp2(_))
    | (BinStringOp2, BinStringOp1(_))
    | (Cons1, Cons2(_))
    | (Cons2, Cons1(_)) => None
    | (Closure, _) => Some(obj)
    | (tag, Closure(_, c)) => unwrap({...obj, ctx: c}, tag)
    | (Filter, _) => Some(obj)
    | (tag, Filter(_, c)) => unwrap({...obj, ctx: c}, tag)
    | (Cast, _) => Some(obj)
    | (tag, Cast(c, _, _)) => unwrap({...obj, ctx: c}, tag)
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
        (env: ClosureEnvironment.t, fact: FilterAction.t, d: DHExp.t)
        : m(list(EvalObj.t)) => {
  let wrap = (fctx: EvalCtx.t => EvalCtx.t, ld: list(EvalObj.t)) =>
    List.map((obj: EvalObj.t) => {...obj, ctx: fctx(obj.ctx)}, ld);

  let go = (dcs: list((DHExp.t, EvalCtx.t => EvalCtx.t))) => {
    let* is_final = {
      let f = (pr, d) => {
        let* r = transition(~env, ~fact, ~pause=false, d);
        let* pr = pr;
        (pr && is_final(r)) |> return;
      };
      dcs |> List.map(fst) |> List.fold_left(f, true |> return);
    };
    if (is_final) {
      [EvalObj.mk(env, fact, Mark, d)] |> return;
    } else {
      List.fold_left(
        (rc, (d, fc)) => {
          let* c = decompose(env, fact, d);
          let* rc = rc;
          rc @ wrap(fc, c) |> return;
        },
        return([]),
        dcs,
      );
    };
  };

  switch (d) {
  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Closure(_, Filter(_, Fun(_)))
  | Tag(_)
  | FreeVar(_)
  | InvalidText(_)
  | EmptyHole(_)
  | ExpandingKeyword(_) => [] |> return
  | Closure(_, Fun(_))
  | Fun(_)
  | ApBuiltin(_)
  | FixF(_, _, _)
  | BoundVar(_) => [EvalObj.mk(env, fact, Mark, d)] |> return
  | Ap(d1, d2) => go([(d1, c => Ap1(c, d2)), (d2, c => Ap2(d1, c))])
  | Closure(env', d) =>
    let* env = env |> ClosureEnvironment.union(env') |> with_eig;
    let* ld = decompose(env, fact, d);
    wrap(c => Closure(env', c), ld) |> return;
  | Filter(fact', d) =>
    let* ld = decompose(env, fact', d);
    wrap(c => Filter(fact', c), ld) |> return;
  | Cast(d, ty, ty') =>
    let* ld = decompose(env, fact, d);
    wrap(c => Cast(c, ty, ty'), ld) |> return;
  | NonEmptyHole(reason, u, i, d1) =>
    go([(d1, c => NonEmptyHole(reason, u, i, c))])
  | BinBoolOp(op, d1, d2) =>
    go([
      (d1, c => BinBoolOp1(op, c, d2)),
      (d2, c => BinBoolOp2(op, d1, c)),
    ])
  | BinIntOp(op, d1, d2) =>
    go([(d1, c => BinIntOp1(op, c, d2)), (d2, c => BinIntOp2(op, d1, c))])
  | BinFloatOp(op, d1, d2) =>
    go([
      (d1, c => BinFloatOp1(op, c, d2)),
      (d2, c => BinFloatOp2(op, d1, c)),
    ])
  | BinStringOp(op, d1, d2) =>
    go([
      (d1, c => BinStringOp1(op, c, d2)),
      (d2, c => BinStringOp2(op, d1, c)),
    ])
  | Cons(d1, d2) =>
    go([(d1, c => Cons1(c, d2)), (d2, c => Cons2(d1, c))])
  | FailedCast(d1, ty1, ty2) => go([(d1, c => FailedCast(c, ty1, ty2))])
  | Tuple(ds) =>
    let rec walk = (ld, rd, rc) =>
      switch (rd) {
      | [] => rc
      | [hd, ...tl] =>
        let rc = rc @ [(hd, (c => EvalCtx.Tuple(c, (ld, tl))))];
        walk(ld @ [hd], tl, rc);
      };
    go(walk([], ds, []));
  | ListLit(m, i, e, t, ds) =>
    let rec walk = (ld, rd, rc) =>
      switch (rd) {
      | [] => rc
      | [hd, ...tl] =>
        let rc =
          rc @ [(hd, (c => EvalCtx.ListLit(m, i, e, t, c, (ld, tl))))];
        walk(ld @ [hd], tl, rc);
      };
    go(walk([], ds, []));
  | Sequence(d1, d2) => go([(d1, c => Sequence(c, d2))])
  | Let(dp, d1, d2) => go([(d1, c => Let(dp, c, d2))])
  | Prj(d, n) => go([(d, c => Prj(c, n))])
  | Inj(ty, side, d1) => go([(d1, c => Inj(ty, side, c))])
  | InvalidOperation(d1, err) => go([(d1, c => InvalidOperation(c, err))])
  | ConsistentCase(Case(d1, rule, n)) =>
    go([(d1, c => ConsistentCase(Case(c, rule, n)))])
  | InconsistentBranches(u, i, Case(d1, rule, n)) =>
    go([(d1, c => InconsistentBranches(u, i, Case(c, rule, n)))])
  };
};

let rec compose = (ctx: EvalCtx.t, d: DHExp.t): DHExp.t => {
  switch (ctx) {
  | Mark => d
  | Closure(env, ctx) => Closure(env, compose(ctx, d))
  | Filter(f, ctx) => Filter(f, compose(ctx, d))
  | Sequence(ctx, d2) => Sequence(compose(ctx, d), d2)
  | Ap1(ctx1, d1) => Ap(compose(ctx1, d), d1)
  | Ap2(d1, ctx1) => Ap(d1, compose(ctx1, d))
  | BinBoolOp1(op, ctx1, d1) => BinBoolOp(op, compose(ctx1, d), d1)
  | BinBoolOp2(op, d1, ctx1) => BinBoolOp(op, d1, compose(ctx1, d))
  | BinIntOp1(op, ctx1, d1) => BinIntOp(op, compose(ctx1, d), d1)
  | BinIntOp2(op, d1, ctx1) => BinIntOp(op, d1, compose(ctx1, d))
  | BinFloatOp1(op, ctx1, d1) => BinFloatOp(op, compose(ctx1, d), d1)
  | BinFloatOp2(op, d1, ctx1) => BinFloatOp(op, d1, compose(ctx1, d))
  | BinStringOp1(op, ctx1, d1) => BinStringOp(op, compose(ctx1, d), d1)
  | BinStringOp2(op, d1, ctx1) => BinStringOp(op, d1, compose(ctx1, d))
  | Cons1(ctx1, d1) => Cons(compose(ctx1, d), d1)
  | Cons2(d1, ctx1) => Cons(d1, compose(ctx1, d))
  | Tuple(ctx, (ld, rd)) => Tuple(ld @ [compose(ctx, d), ...rd])
  | ListLit(m, i, e, t, ctx, (ld, rd)) =>
    ListLit(m, i, e, t, ld @ [compose(ctx, d), ...rd])
  | Let(dp, ctx1, d1) => Let(dp, compose(ctx1, d), d1)
  | Prj(ctx, n) => Prj(compose(ctx, d), n)
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

let step = (~pause: bool, obj: EvalObj.t): m(t) => {
  let* r = transition(~env=obj.env, ~fact=obj.flt, ~pause, obj.exp);
  let d = compose(obj.ctx, unbox(r));
  switch (r) {
  | Step(_) => Step(d) |> return
  | BoxedValue(_) => BoxedValue(d) |> return
  | Indet(_) => Indet(d) |> return
  };
};

let init = (d: DHExp.t) => {
  print_endline("======== init BEGIN =========");
  let (es, r) = step(~pause=true, EvalObj.init(d), EvaluatorState.init);
  print_endline("======== init END =========");
  r
  |> sexp_of_t
  |> Sexplib.Sexp.to_string_hum
  |> (s => print_endline("init returns r = " ++ s));
  (es, r);
};

let step = (obj: EvalObj.t) => {
  print_endline("======== step BEGIN =========");
  let r = step(~pause=false, obj, EvaluatorState.init);
  print_endline("======== step END =========");
  r;
};

let decompose = (d: DHExp.t) => {
  print_endline("======== decompose BEGIN =========");
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  let r = decompose(env, Keep, d, es);
  print_endline("======== decompose END =========");
  r;
};
