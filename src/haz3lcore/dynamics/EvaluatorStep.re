open Sexplib.Std;
open Util;
module Monad = EvaluatorMonad;
open Monad;
open Monad.Syntax;

module Filter = DHExp.Filter;

module FilterAction = DHExp.FilterAction;

module FilterEnvironment = DHExp.FilterEnvironment;

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
    | ListConcat1
    | ListConcat2
    | Prj
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
    | Filter(DHExp.FilterEnvironment.t, t)
    | Sequence(t, DHExp.t)
    | Let(DHPat.t, t, DHExp.t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(TermBase.UExp.op_bin_bool, t, DHExp.t)
    | BinBoolOp2(TermBase.UExp.op_bin_bool, DHExp.t, t)
    | BinIntOp1(TermBase.UExp.op_bin_int, t, DHExp.t)
    | BinIntOp2(TermBase.UExp.op_bin_int, DHExp.t, t)
    | BinFloatOp1(TermBase.UExp.op_bin_float, t, DHExp.t)
    | BinFloatOp2(TermBase.UExp.op_bin_float, DHExp.t, t)
    | BinStringOp1(TermBase.UExp.op_bin_string, t, DHExp.t)
    | BinStringOp2(TermBase.UExp.op_bin_string, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | ListLit(
        MetaVar.t,
        MetaVarInst.t,
        Typ.t,
        t,
        (list(DHExp.t), list(DHExp.t)),
      )
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | ListConcat1(t, DHExp.t)
    | ListConcat2(DHExp.t, t)
    | Prj(t, int)
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

/**
  Alias for EvaluatorMonad.
 */
type m('a) = EvaluatorMonad.t('a);

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t,
    ctx: EvalCtx.t,
    act: FilterAction.t,
    exp: DHExp.t,
  };

  let mk = (env, ctx, act, exp) => {env, ctx, act, exp};

  let mark = (env, act, exp) => {env, ctx: Mark, act, exp};

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
    | (ListConcat1, ListConcat1(c, _))
    | (ListConcat2, ListConcat2(_, c))
    | (Prj, Prj(c, _)) => Some({...obj, ctx: c})
    | (Tuple(n), Tuple(c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (ListLit(n), ListLit(_, _, _, c, (ld, _))) =>
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
    | (Cons2, Cons1(_))
    | (ListConcat1, ListConcat2(_))
    | (ListConcat2, ListConcat1(_)) => None
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

  let wrap = (f: EvalCtx.t => EvalCtx.t, obj: t) => {
    ...obj,
    ctx: obj.ctx |> f,
  };
};

module Capture = {
  module Environment: {
    include
       (module type of VarBstMap.Ordered) with
        type t_('a) = VarBstMap.Ordered.t_('a);

    type t = t_(unit);

    let strip: DH.Environment.t => t;
  } = {
    include VarBstMap.Ordered;

    type t = t_(unit);

    let strip = (env: DH.Environment.t) => {
      DH.Environment.mapo(_ => (), env);
    };
  };

  type t = Environment.t;

  let rec provides = (dp: DHPat.t): t => {
    switch (dp) {
    | Var(x) => Environment.extend(Environment.empty, (x, ()))
    | Ap(dp1, dp2) => Environment.union(provides(dp2), provides(dp1))
    | Tuple(dps) =>
      dps
      |> List.fold_left(
           (acc, dp) => Environment.union(provides(dp), acc),
           Environment.empty,
         )
    | ListLit(_, dps) =>
      dps
      |> List.fold_left(
           (acc, dp) => Environment.union(provides(dp), acc),
           Environment.empty,
         )
    | Cons(dp1, dp2) => Environment.union(provides(dp2), provides(dp1))
    | _ => Environment.empty
    };
  };

  let rec analyze = (env: Environment.t, d: DHExp.t): Environment.t => {
    switch (d) {
    | BoundVar(x) =>
      let r = x |> Environment.lookup(env);
      switch (r) {
      | Some(_) => Environment.empty
      | None => Environment.singleton((x, ()))
      };
    | Sequence(d1, d2) =>
      Environment.union(analyze(env, d2), analyze(env, d1))
    | Let(dp, d1, d2) =>
      let env' = Environment.union(provides(dp), env);
      Environment.union(analyze(env, d1), analyze(env', d2));
    | FixF(f, _, d') =>
      let env' = Environment.extend(env, (f, ()));
      analyze(env', d');
    | Fun(dp, _, d1, _) =>
      let env' = Environment.union(provides(dp), env);
      analyze(env', d1);
    | Ap(d1, d2) => Environment.union(analyze(env, d1), analyze(env, d2))
    | ApBuiltin(_, args) =>
      args
      |> List.fold_left(
           (acc, arg) => Environment.union(analyze(env, arg), acc),
           Environment.empty,
         )

    | TestLit(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | Constructor(_) => Environment.empty

    | BinBoolOp(_, d1, d2) =>
      Environment.union(analyze(env, d1), analyze(env, d2))
    | BinIntOp(_, d1, d2) =>
      Environment.union(analyze(env, d1), analyze(env, d2))
    | BinFloatOp(_, d1, d2) =>
      Environment.union(analyze(env, d1), analyze(env, d2))
    | BinStringOp(_, d1, d2) =>
      Environment.union(analyze(env, d1), analyze(env, d2))
    | ListConcat(d1, d2) =>
      Environment.union(analyze(env, d1), analyze(env, d2))
    | Tuple(ds) =>
      ds
      |> List.fold_left(
           (acc, d) => {acc |> Environment.union(analyze(env, d))},
           Environment.empty,
         )

    | Prj(targ, _) => analyze(env, targ)
    | Cons(d1, d2) => Environment.union(analyze(env, d2), analyze(env, d1))
    | ListLit(_, _, _, lst) =>
      lst
      |> List.fold_left(
           (acc, d) => {acc |> Environment.union(analyze(env, d))},
           Environment.empty,
         )
    | ConsistentCase(Case(d1, rules, _)) =>
      rules
      |> List.fold_left(
           (acc: Environment.t, DHExp.Rule(dp, d)) => {
             let env' = Environment.union(provides(dp), env);
             acc |> Environment.union(analyze(env', d));
           },
           analyze(env, d1),
         )
    | Closure(env', d1) =>
      let env' = env' |> ClosureEnvironment.map_of |> Environment.strip;
      let env'' = Environment.union(env', env);
      analyze(env'', d1);
    | Filter(_, d1) => analyze(env, d1)
    | InconsistentBranches(_, _, Case(d1, rules, _)) =>
      rules
      |> List.fold_left(
           (acc: Environment.t, DHExp.Rule(dp, d)) => {
             let env' = Environment.union(provides(dp), env);
             acc |> Environment.union(analyze(env', d));
           },
           analyze(env, d1),
         )
    | EmptyHole(_) => Environment.empty
    | NonEmptyHole(_, _, _, d1) => analyze(env, d1)
    | FreeVar(_)
    | ExpandingKeyword(_)
    | InvalidText(_) => Environment.empty
    | Cast(d1, _, _) => analyze(env, d1)
    | FailedCast(d1, _, _) => analyze(env, d1)
    | InvalidOperation(d1, _) => analyze(env, d1)
    };
  };

  let is_closed = (d: DHExp.t): bool => {
    d |> analyze(Environment.empty) |> Environment.is_empty;
  };

  let capture = (d: DHExp.t, env: ClosureEnvironment.t): ClosureEnvironment.t => {
    let renv = analyze(Environment.empty, d);
    env
    |> ClosureEnvironment.filter_keep_id(((var, _)) =>
         Environment.lookup(renv, var) |> Option.is_some
       );
  };
};

module Transition = {
  module Result = {
    type t =
      | Indet(DHExp.t)
      | BoxedValue(DHExp.t)
      | Step(DHExp.t);
  };

  let rec transition = (env: ClosureEnvironment.t, d: DHExp.t): m(Result.t) => {
    open Evaluator;
    open Result;
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
      Step(d) |> return;

    | Sequence(d1, d2) =>
      let* r1 = transition(env, d1);
      switch (r1) {
      | Step(d1') => Step(Sequence(d1', d2)) |> return
      | BoxedValue(_d1)
      /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
       * if evaluated d1 is indet. */
      | Indet(_d1) =>
        /* let* r2 = step(env, d2, opt); */
        /* switch (r2) { */
        /* | BoxedValue(d2) */
        /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
        /* }; */
        Step(d2) |> return
      };

    | Let(dp, d1, d2) =>
      let* r1 = transition(env, d1);
      switch (r1) {
      | Step(d1') => Step(Let(dp, d1', d2)) |> return
      | BoxedValue(d1')
      | Indet(d1') =>
        switch (Evaluator.matches(dp, d1')) {
        | IndetMatch
        | DoesNotMatch => Indet(Let(dp, d1', d2)) |> return
        | Matches(env') =>
          let* env =
            env |> Capture.capture(d2) |> Evaluator.evaluate_extend_env(env');
          Step(Closure(env, d2)) |> return;
        }
      };

    | FixF(f, _, d') =>
      let* env' =
        env
        |> Capture.capture(d)
        |> Evaluator.evaluate_extend_env(Environment.singleton((f, d)));
      Step(Closure(env', d')) |> return;

    | Fun(_) => Step(Closure(Capture.capture(d, env), d)) |> return

    | Ap(d1, d2) =>
      let* r1 = transition(env, d1);
      switch (r1) {
      | Step(d1') => Step(Ap(d1', d2)) |> return
      | BoxedValue(TestLit(id)) =>
        let* r2 = Evaluator.evaluate_test(env, id, d2);
        switch (r2) {
        | Indet(r2) => Indet(r2) |> return
        | BoxedValue(r2) => Step(r2) |> return
        };
      | BoxedValue(Constructor(_)) =>
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
          switch (Evaluator.matches(dp, d2)) {
          | DoesNotMatch
          | IndetMatch => Indet(Ap(d1, d2)) |> return
          | Matches(env') =>
            /* evaluate a closure: extend the closure environment with the
             * new bindings introduced by the function application. */
            let* env =
              closure_env
              |> Capture.capture(d3)
              |> Evaluator.evaluate_extend_env(env');
            Step(Closure(env, d3)) |> return;
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
          Step(
            Closure(env, Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2')),
          )
          |> return
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
      let* r = Evaluator.evaluate_ap_builtin(env, ident, args);
      switch (r) {
      | BoxedValue(d) => Step(d) |> return
      | Indet(d) => Indet(d) |> return
      };

    | TestLit(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | Constructor(_) => BoxedValue(d) |> return

    | BinBoolOp(op, d1, d2) =>
      let* r1 = transition(env, d1);
      switch (r1) {
      | Step(d1') => Step(BinBoolOp(op, d1', d2)) |> return
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (Evaluator.eval_bin_bool_op_short_circuit(op, b1)) {
        | Some(b3) => Step(b3) |> return
        | None =>
          let* r2 = transition(env, d2);
          switch (r2) {
          | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
          | BoxedValue(BoolLit(b2)) =>
            Step(Evaluator.eval_bin_bool_op(op, b1, b2)) |> return
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
      | Step(d1') => Step(BinIntOp(op, d1', d2)) |> return
      | BoxedValue(IntLit(n1) as d1') =>
        let* r2 = transition(env, d2);
        switch (r2) {
        | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
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
          | _ => Step(Evaluator.eval_bin_int_op(op, n1, n2)) |> return
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
          Step(Evaluator.eval_bin_float_op(op, f1, f2)) |> return
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
          Step(Evaluator.eval_bin_string_op(op, s1, s2)) |> return
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

    | ListConcat(d1, d2) =>
      let* r1 = transition(env, d1);
      switch (r1) {
      | Step(d1') => Step(ListConcat(d1', d2)) |> return
      | BoxedValue(d1') =>
        let* r2 = transition(env, d2);
        switch (r2) {
        | Step(d2') => Step(ListConcat(d1', d2')) |> return
        | BoxedValue(d2') =>
          switch (d1', d2') {
          | (ListLit(u, i, ty, ds1), ListLit(_, _, _, ds2)) =>
            Step(ListLit(u, i, ty, ds1 @ ds2)) |> return
          | (Cast(d1, List(ty), List(ty')), d2)
          | (d1, Cast(d2, List(ty), List(ty'))) =>
            transition(env, Cast(ListConcat(d1, d2), List(ty), List(ty')))
          | (ListLit(_), _) =>
            print_endline("InvalidBoxedListLit: " ++ DHExp.show(d2));
            raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
          | _ =>
            print_endline("InvalidBoxedListLit: " ++ DHExp.show(d1));
            raise(EvaluatorError.Exception(InvalidBoxedListLit(d1)));
          }
        | Indet(d2') => Indet(ListConcat(d1', d2')) |> return
        };
      | Indet(d1') => Step(ListConcat(d1', d2)) |> return
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
      if (n < 0) {
        return(
          Indet(
            InvalidOperation(d, InvalidOperationError.InvalidProjection),
          ),
        );
      } else {
        let* r = transition(env, targ);
        switch (r) {
        | Step(Tuple(ds) as rv) =>
          if (n >= List.length(ds)) {
            Step(
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
        | Indet(Closure(_, Tuple(ds)) as rv)
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
            Step(Closure(env, Cast(Prj(targ', n), ty, ty'))) |> return;
          }
        | Step(d) => Step(Prj(d, n)) |> return
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
        | ListLit(u, i, ty, ds) =>
          BoxedValue(ListLit(u, i, ty, [d1, ...ds])) |> return
        | Cons(_)
        | Cast(ListLit(_), List(_), List(_)) =>
          BoxedValue(Cons(d1, d2)) |> return
        | _ =>
          print_endline("InvalidBoxedListLit");
          raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
        }
      };

    | ListLit(u, i, ty, ds) =>
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

      let d' = DHExp.ListLit(u, i, ty, ds');

      switch (tag) {
      | Step(_) => Step(d')
      | Indet(_) => Indet(d')
      | BoxedValue(_) => BoxedValue(d')
      };

    | ConsistentCase(Case(d1, rules, n)) =>
      transition_case(env, d1, rules, n)

    /* Generalized closures evaluate to themselves. Only
       lambda closures are BoxedValues; other closures are all Indet. */
    | Closure(env', d') =>
      switch (d') {
      | Fun(_) => BoxedValue(d) |> return
      | _ =>
        let* env = ClosureEnvironment.union(env', env) |> with_eig;
        let* r = transition(env, d');
        switch (r) {
        | Step(d) =>
          if (Capture.is_closed(d)) {
            Step(d) |> return;
          } else {
            Step(Closure(env, d)) |> return;
          }
        | BoxedValue(d) => BoxedValue(d) |> return
        | Indet(d) => Indet(d) |> return
        };
      }

    | Filter(fenv, d) =>
      let* r = transition(env, d);
      switch (r) {
      | BoxedValue(_)
      | Indet(_) => r |> return
      | Step(Filter(fenv', d')) =>
        let fenv'' = FilterEnvironment.extends(fenv', fenv);
        Step(Filter(fenv'', d')) |> return;
      | Step(d') =>
        let* r' = transition(env, d');
        switch (r') {
        | BoxedValue(_)
        | Indet(_) => Step(d') |> return
        | Step(_) => Step(Filter(fenv, d')) |> return
        };
      };

    /* Hole expressions */
    | InconsistentBranches(u, i, Case(d1, rules, n)) =>
      //TODO: revisit this, consider some kind of dynamic casting
      Indet(Closure(env, InconsistentBranches(u, i, Case(d1, rules, n))))
      |> return

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
        switch (
          Evaluator.ground_cases_of(ty),
          Evaluator.ground_cases_of(ty'),
        ) {
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
          let d' =
            DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
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
          let d' =
            DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
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
  and transition_case =
      (
        env: ClosureEnvironment.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t) => {
    open Result;
    let* rscrut = transition(env, scrut);
    switch (rscrut) {
    | BoxedValue(scrut) =>
      transition_rule(env, scrut, rules, current_rule_index)
    | Indet(scrut) => eval_rule(env, scrut, rules, current_rule_index)
    | Step(scrut) =>
      Step(ConsistentCase(Case(scrut, rules, current_rule_index)))
      |> Monad.return
    };
  }
  and transition_rule =
      (
        env: ClosureEnvironment.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t) => {
    Result.(
      switch (List.nth_opt(rules, current_rule_index)) {
      | None =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        Indet(Closure(env, ConsistentCase(case))) |> Monad.return;
      | Some(Rule(dp, d)) =>
        switch (Evaluator.matches(dp, scrut)) {
        | IndetMatch =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          Indet(Closure(env, ConsistentCase(case))) |> Monad.return;
        | Matches(env') =>
          // extend environment with new bindings introduced
          let* env =
            env |> Capture.capture(d) |> Evaluator.evaluate_extend_env(env');
          Step(Closure(env, d)) |> Monad.return;
        // by the rule and evaluate the expression.
        | DoesNotMatch =>
          transition_rule(env, scrut, rules, current_rule_index + 1)
        }
      }
    );
  }
  and eval_rule =
      (
        env: ClosureEnvironment.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t) => {
    Result.(
      switch (List.nth_opt(rules, current_rule_index)) {
      | None =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        Indet(Closure(env, ConsistentCase(case))) |> Monad.return;
      | Some(Rule(dp, d)) =>
        switch (Evaluator.matches(dp, scrut)) {
        | IndetMatch =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          Indet(Closure(env, ConsistentCase(case))) |> Monad.return;
        | Matches(env') =>
          // extend environment with new bindings introduced
          let* env =
            env |> Capture.capture(d) |> Evaluator.evaluate_extend_env(env');
          transition(env, d);
        // by the rule and evaluate the expression.
        | DoesNotMatch => eval_rule(env, scrut, rules, current_rule_index + 1)
        }
      }
    );
  };
};

module Decompose = {
  module Result = {
    type cls =
      | Indet
      | BoxedValue
      | Eval
      | Step;

    type t =
      | Indet
      | BoxedValue
      | Eval(EvalObj.t)
      | Step(list(EvalObj.t));

    let map = (f: EvalObj.t => EvalObj.t, r: t) => {
      switch (r) {
      | Indet => Indet
      | BoxedValue => BoxedValue
      | Eval(obj) => Eval(f(obj))
      | Step(objs) => Step(List.map(f, objs))
      };
    };

    let unbox = (r: t): list(EvalObj.t) => {
      switch (r) {
      | Indet
      | BoxedValue => []
      | Eval(obj) => [obj]
      | Step(objs) => objs
      };
    };

    let return = (act: DHExp.FilterAction.t, obj: EvalObj.t): t => {
      switch (act) {
      | Eval => Eval(obj)
      | Step => Step([obj])
      };
    };
  };

  let rec decompose =
          (
            env: ClosureEnvironment.t,
            flt: FilterEnvironment.t,
            act: FilterAction.t,
            exp: DHExp.t,
          )
          : Monad.t(Result.t) => {
    let act = FilterEnvironment.matches(exp, act, flt);
    let decompose = (~env=env, ~flt=flt, ~act=act, exp) =>
      decompose(env, flt, act, exp);

    module Return = {
      type t =
        | Operator
        | Constructor;

      let merge =
          (cat: t, rs: list((Result.t, EvalCtx.t => EvalCtx.t))): Result.t => {
        let merge_cls = (rc: Result.cls, r: Result.t): Result.cls => {
          switch (rc, r) {
          | (_, Step(_))
          | (Step, _) => Step
          | (_, Eval(_))
          | (Eval, _) => Eval
          | (_, Indet)
          | (Indet, _) => Indet
          | (BoxedValue, BoxedValue) => BoxedValue
          };
        };

        let cls =
          rs
          |> List.fold_left((rc, (r, _)) => merge_cls(rc, r), BoxedValue);

        switch (act, cls) {
        | (Step, Step)
        | (Step, Eval)
        | (Eval, Step) =>
          let folder = (ac, (r: Result.t, f)) =>
            switch (r) {
            | Indet
            | BoxedValue => ac
            | Eval(obj) => [obj |> EvalObj.wrap(f), ...ac]
            | Step(objs) => List.map(EvalObj.wrap(f), objs) @ ac
            };
          let rs = rs |> List.fold_left(folder, []);
          Step(rs);
        | (Eval, Eval)
        | (Eval, BoxedValue) =>
          let env = env |> Capture.capture(exp);
          switch (cat) {
          | Operator => Eval(EvalObj.mark(env, Eval, exp))
          | Constructor => BoxedValue
          };
        | (Step, BoxedValue) =>
          let env = env |> Capture.capture(exp);
          switch (cat) {
          | Operator => Step([EvalObj.mark(env, Step, exp)])
          | Constructor => BoxedValue
          };
        | (Eval, Indet)
        | (Step, Indet) => Indet
        };
      };

      let wrap = f => Result.map(EvalObj.wrap(f));

      let mark = act => {
        let env = env |> Capture.capture(exp);
        EvalObj.mark(env, act, exp) |> Result.return(act) |> Monad.return;
      };

      let merge = (cat: t, rs) => rs |> merge(cat) |> Monad.return;

      let boxed = Result.BoxedValue |> Monad.return;

      let indet = Result.Indet |> Monad.return;
    };

    let* r = Transition.transition(env, exp);
    switch (r) {
    | BoxedValue(_) => Return.boxed
    | Indet(_) => Return.indet
    | Step(_) =>
      switch (exp) {
      | EmptyHole(_) => Return.indet
      | NonEmptyHole(_) => Return.indet
      | ExpandingKeyword(_) => Return.indet
      | FreeVar(_) => Return.indet
      | InvalidText(_) => Return.indet
      | InconsistentBranches(_) => Return.indet
      | FailedCast(_) => Return.indet
      | InvalidOperation(_) => Return.indet
      | Closure(_, Fun(_)) => Return.boxed
      | Closure(env', d1) =>
        let* env = ClosureEnvironment.union(env', env) |> with_eig;
        decompose(~env, d1) >>| Return.wrap(c => Closure(env', c));
      | Filter(flt', d1) =>
        let flt = DHExp.FilterEnvironment.extends(flt', flt);
        decompose(~flt, d1) >>| Return.wrap(c => Filter(flt', c));
      | Cast(d1, ty, ty') =>
        decompose(d1) >>| Return.wrap(c => Cast(c, ty, ty'))
      | BoundVar(_) => Return.mark(act)
      | Sequence(d1, d2) =>
        let* r1 = decompose(d1);
        switch (r1) {
        | BoxedValue
        | Eval(_) => Return.mark(act)
        | Indet
        | Step(_) => r1 |> Return.wrap(c => Sequence(c, d2)) |> Monad.return
        };
      | Let(dp, d1, d2) =>
        let* r1 = decompose(d1);
        switch (r1) {
        | BoxedValue
        | Eval(_) => Return.mark(act)
        | Indet
        | Step(_) => r1 |> Return.wrap(c => Let(dp, c, d2)) |> Monad.return
        };
      | FixF(_) => Return.mark(act)
      | Fun(_) => Return.mark(act)
      | Ap(d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        switch (r1, r2) {
        | (BoxedValue, BoxedValue)
        | (BoxedValue, Eval(_))
        | (Eval(_), BoxedValue)
        | (Eval(_), Eval(_)) => Return.mark(act)
        | (Indet, _)
        | (_, Indet)
        | (Step(_), _)
        | (_, Step(_)) =>
          [(r1, (c => Ap1(c, d2))), (r2, (c => Ap2(d1, c)))]
          |> Return.merge(Return.Operator)
        };
      | ApBuiltin(_) => Return.mark(act)
      | TestLit(_)
      | BoolLit(_)
      | IntLit(_)
      | FloatLit(_)
      | StringLit(_)
      | Constructor(_) => Return.boxed
      | BinBoolOp(op, d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        [
          (r1, (c => BinBoolOp1(op, c, d2))),
          (r2, (c => BinBoolOp2(op, d1, c))),
        ]
        |> Return.merge(Return.Operator);
      | BinIntOp(op, d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        [
          (r1, (c => BinIntOp1(op, c, d2))),
          (r2, (c => BinIntOp2(op, d1, c))),
        ]
        |> Return.merge(Return.Operator);
      | BinFloatOp(op, d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        [
          (r1, (c => BinFloatOp1(op, c, d2))),
          (r2, (c => BinFloatOp2(op, d1, c))),
        ]
        |> Return.merge(Return.Operator);
      | BinStringOp(op, d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        [
          (r1, (c => BinStringOp1(op, c, d2))),
          (r2, (c => BinStringOp2(op, d1, c))),
        ]
        |> Return.merge(Return.Operator);
      | Tuple(ds) =>
        let rec walk = (ld, rd, rs) => {
          switch (rd) {
          | [] => rs
          | [d, ...rd] =>
            let* r = decompose(d);
            let* rs = rs;
            let rs = [(r, (c => EvalCtx.Tuple(c, (ld, rd)))), ...rs];
            walk([d, ...ld], rd, return(rs));
          };
        };
        let* rs = walk([], ds, return([]));
        rs |> Return.merge(Return.Constructor);
      | Prj(targ, n) => decompose(targ) >>| Return.wrap(c => Prj(c, n))
      | Cons(d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        [(r1, (c => Cons1(c, d2))), (r2, (c => Cons2(d1, c)))]
        |> Return.merge(Return.Operator);
      | ListConcat(d1, d2) =>
        let* r1 = decompose(d1);
        let* r2 = decompose(d2);
        [(r1, (c => ListConcat1(c, d2))), (r2, (c => ListConcat2(d1, c)))]
        |> Return.merge(Return.Operator);
      | ListLit(u, i, ty, lst) =>
        let rec walk = (ld, rd, rs) => {
          switch (rd) {
          | [] => rs
          | [d, ...rd] =>
            let* r = decompose(d);
            let* rs = rs;
            let rs = [
              (r, (c => EvalCtx.ListLit(u, i, ty, c, (ld, rd)))),
              ...rs,
            ];
            walk([d, ...ld], rd, return(rs));
          };
        };
        let* rs = walk([], lst, return([]));
        rs |> Return.merge(Return.Constructor);
      | ConsistentCase(Case(d1, rules, i)) =>
        let* r1 = decompose(d1);
        switch (r1) {
        | BoxedValue
        | Eval(_) => Return.mark(act)
        | Indet
        | Step(_) =>
          r1
          |> Return.wrap(c => ConsistentCase(Case(c, rules, i)))
          |> Monad.return
        };
      }
    };
  };
};

let rec compose = (ctx: EvalCtx.t, d: DHExp.t): m(DHExp.t) => {
  open DHExp;
  let return: DHExp.t => m(DHExp.t) = return;
  let rec rev_concat = (ls: list('a), rs: list('a)) => {
    switch (ls) {
    | [] => rs
    | [hd, ...tl] => rev_concat(tl, [hd, ...rs])
    };
  };
  switch (ctx) {
  | Mark => d |> return
  | Closure(env, ctx) =>
    let* d = compose(ctx, d);
    let flist = Capture.analyze(Environment.empty, d);
    if (Environment.is_empty(flist)) {
      d |> return;
    } else {
      Closure(env, d) |> return;
    };
  | Filter(fenv, ctx) =>
    let+ d = compose(ctx, d);
    switch (d) {
    | Filter(fenv', d) =>
      let fenv'' = FilterEnvironment.extends(fenv', fenv);
      Filter(fenv'', d);
    | _ => Filter(fenv, d)
    };
  | Sequence(ctx, d2) =>
    let+ d1 = compose(ctx, d);
    Sequence(d1, d2);
  | Ap1(ctx, d2) =>
    let+ d1 = compose(ctx, d);
    Ap(d1, d2);
  | Ap2(d1, ctx) =>
    let+ d2 = compose(ctx, d);
    Ap(d1, d2);
  | BinBoolOp1(op, ctx, d2) =>
    let+ d1 = compose(ctx, d);
    BinBoolOp(op, d1, d2);
  | BinBoolOp2(op, d1, ctx) =>
    let+ d2 = compose(ctx, d);
    BinBoolOp(op, d1, d2);
  | BinIntOp1(op, ctx, d2) =>
    let+ d1 = compose(ctx, d);
    BinIntOp(op, d1, d2);
  | BinIntOp2(op, d1, ctx) =>
    let+ d2 = compose(ctx, d);
    BinIntOp(op, d1, d2);
  | BinFloatOp1(op, ctx, d2) =>
    let+ d1 = compose(ctx, d);
    BinFloatOp(op, d1, d2);
  | BinFloatOp2(op, d1, ctx) =>
    let+ d2 = compose(ctx, d);
    BinFloatOp(op, d1, d2);
  | BinStringOp1(op, ctx, d2) =>
    let+ d1 = compose(ctx, d);
    BinStringOp(op, d1, d2);
  | BinStringOp2(op, d1, ctx) =>
    let+ d2 = compose(ctx, d);
    BinStringOp(op, d1, d2);
  | Cons1(ctx, d2) =>
    let+ d1 = compose(ctx, d);
    Cons(d1, d2);
  | Cons2(d1, ctx) =>
    let+ d2 = compose(ctx, d);
    Cons(d1, d2);
  | ListConcat1(ctx, d2) =>
    let+ d1 = compose(ctx, d);
    ListConcat(d1, d2);
  | ListConcat2(d1, ctx) =>
    let+ d2 = compose(ctx, d);
    ListConcat(d1, d2);
  | Tuple(ctx, (ld, rd)) =>
    let+ d = compose(ctx, d);
    Tuple(rev_concat(ld, [d, ...rd]));
  | ListLit(m, i, t, ctx, (ld, rd)) =>
    let+ d = compose(ctx, d);
    ListLit(m, i, t, rev_concat(ld, [d, ...rd]));
  | Let(dp, ctx, d1) =>
    let+ d = compose(ctx, d);
    Let(dp, d, d1);
  | Prj(ctx, n) =>
    let+ d = compose(ctx, d);
    Prj(d, n);
  | Cast(ctx, ty1, ty2) =>
    let+ d = compose(ctx, d);
    Cast(d, ty1, ty2);
  | FailedCast(ctx, ty1, ty2) =>
    let+ d = compose(ctx, d);
    FailedCast(d, ty1, ty2);
  | InvalidOperation(ctx, err) =>
    let+ d = compose(ctx, d);
    InvalidOperation(d, err);
  | NonEmptyHole(reason, u, i, ctx) =>
    let+ d = compose(ctx, d);
    NonEmptyHole(reason, u, i, d);
  | ConsistentCase(Case(ctx, rule, n)) =>
    let+ d = compose(ctx, d);
    ConsistentCase(Case(d, rule, n));
  | InconsistentBranches(u, i, Case(ctx, rule, n)) =>
    let+ d = compose(ctx, d);
    InconsistentBranches(u, i, Case(d, rule, n));
  };
};

let step = (obj: EvalObj.t): m(EvaluatorResult.t) => {
  let* r =
    switch (obj.act) {
    | Eval => Evaluator.evaluate_closure(obj.env, obj.exp)
    | Step =>
      let* r = Transition.transition(obj.env, obj.exp);
      switch (r) {
      | Step(d)
      | BoxedValue(d) => EvaluatorResult.BoxedValue(d) |> return
      | Indet(d) => EvaluatorResult.Indet(d) |> return
      };
    };
  let* d = compose(obj.ctx, EvaluatorResult.unbox(r));
  switch (r) {
  | BoxedValue(_) => EvaluatorResult.BoxedValue(d) |> return
  | Indet(_) => EvaluatorResult.Indet(d) |> return
  };
};

let evaluate_with_history = (d: DHExp.t) => {
  let rec go =
          (env: ClosureEnvironment.t, d: DHExp.t, rs: m(list(DHExp.t)))
          : m(list(DHExp.t)) => {
    let* rs = rs;
    let* r = Transition.transition(env, d);
    switch (r) {
    | Step(d) => go(env, d, [d, ...rs] |> return)
    | BoxedValue(_) => rs |> return
    | Indet(_) => rs |> return
    };
  };
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  let (_, rs) = go(env, d, [] |> return, es);
  rs;
};

let step = (obj: EvalObj.t) => {
  step(obj, EvaluatorState.init);
};

let step = Core.Memo.general(~cache_size_bound=1000, step);

let step = (obj: EvalObj.t): ProgramResult.t => {
  let (es, d) = step(obj);
  switch (d) {
  | BoxedValue(d) => (BoxedValue(d), es)
  | Indet(d) => (Indet(d), es)
  | exception (EvaluatorError.Exception(_reason)) =>
    //HACK(andrew): supress exceptions for release
    //raise(EvalError(reason))
    print_endline("Interface.step EXCEPTION");
    (Indet(InvalidText(Id.invalid, 0, "EXCEPTION")), EvaluatorState.init);
  | exception _ =>
    print_endline("Other evaluation exception raised (stack overflow?)");
    (Indet(InvalidText(Id.invalid, 0, "EXCEPTION")), EvaluatorState.init);
  };
};

let decompose = (d: DHExp.t) => {
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  let (_, rs) = Decompose.decompose(env, [], Step, d, es);
  Decompose.Result.unbox(rs);
};

module Stepper = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type step = {
    d: DHExp.t,
    step: EvalObj.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: DHExp.t,
    previous: list(step),
    next: list(EvalObj.t),
  };

  let mk = d => {
    {current: d, previous: [], next: decompose(d)};
  };

  let step_forward = (e: EvalObj.t, s: t) => {
    let current = ProgramResult.get_dhexp(step(e));
    {
      current,
      previous: [{d: s.current, step: e}, ...s.previous],
      next: decompose(current),
    };
  };

  let step_backward = (s: t) =>
    switch (s.previous) {
    | [] => failwith("cannot step backwards")
    | [x, ...xs] => {current: x.d, previous: xs, next: decompose(x.d)}
    };

  let update_expr = (d: DHExp.t, _: t) => {
    current: d,
    previous: [],
    next: decompose(d),
  };

  let get_justification: DHExp.t => string =
    fun
    | EmptyHole(_)
    | NonEmptyHole(_)
    | ExpandingKeyword(_)
    | InvalidText(_)
    | InconsistentBranches(_)
    | Closure(_)
    | Filter(_)
    | FixF(_)
    | Fun(_)
    | TestLit(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | ListLit(_)
    | Cons(_)
    | ListConcat(_)
    | Tuple(_)
    | Prj(_)
    | Constructor(_)
    | ConsistentCase(_)
    | Cast(_)
    | FailedCast(_)
    | InvalidOperation(_)
    | FreeVar(_) => "unidentified step"
    | BoundVar(_) => "by variable lookup"
    | Sequence(_) => "by sequence"
    | Let(_) => "by substitution"
    | Ap(_) => "by application [TODO]"
    | ApBuiltin(_) => "by builtin application [TODO]"
    | BinBoolOp(_) => "by boolean logic"
    | BinIntOp(_) => "by arithmetic"
    | BinFloatOp(_) => "by arithmetic"
    | BinStringOp(_) => "by string operation";

  let get_history = stepper =>
    List.map(s => (s.d, get_justification(s.step.exp)), stepper.previous);
};
