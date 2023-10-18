open Sexplib.Std;
open Util;
module Monad = EvaluatorMonad;
open Monad;
open Monad.Syntax;

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
    | Closure(ClosureEnvironment.t, FilterEnvironment.t, t)
    | Sequence(t, DHExp.t)
    | Let(DHPat.t, t, DHExp.t)
    | Filter(Filter.t, t)
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
    flt: FilterEnvironment.t,
    ctx: EvalCtx.t,
    act: FilterAction.t,
    exp: DHExp.t,
  };

  let mk = (env, flt, ctx, act, exp) => {env, flt, ctx, act, exp};

  let mark = (env, flt, act, exp) => {env, flt, ctx: Mark, act, exp};

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
    | (Closure, Closure(_, _, c))
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
    | (tag, Closure(_, _, c)) => unwrap({...obj, ctx: c}, tag)
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

module Transition = {
  module Result = {
    type t =
      | Indet(DHExp.t)
      | BoxedValue(DHExp.t)
      | Step(DHExp.t);
    let unbox =
      fun
      | Indet(d)
      | BoxedValue(d)
      | Step(d) => d;
  };

  let rec transition =
          (
            env: ClosureEnvironment.t,
            fenv: FilterEnvironment.t,
            fact: FilterAction.t,
            d: DHExp.t,
          )
          : m(Result.t) => {
    print_endline("Evaluating: " ++ DHExp.show(d));
    open Evaluator;
    open Result;
    let mact = FilterEnvironment.matches(d, fact, fenv);
    module Return = {
      let builtin = d => {
        switch (mact) {
        | Eval => BoxedValue(d) |> return
        | Pause => Step(d) |> return
        };
      };
      let continue = (~env=env, ~fenv=fenv, ~fact=mact, d) => {
        switch (mact) {
        | Eval => transition(env, fenv, fact, d)
        | Pause => Step(Closure(env, fenv, d)) |> return
        };
      };
    };
    let transition = (~env=env, ~fenv=fenv, ~fact=mact, d: DHExp.t) => {
      transition(env, fenv, fact, d);
    };
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
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) => Step(Sequence(d1, d2)) |> return
      | BoxedValue(_)
      | Indet(_) => Return.continue(d2)
      };

    | Let(dp, d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) => Step(Let(dp, d1, d2)) |> return
      | BoxedValue(d1)
      | Indet(d1) =>
        switch (Evaluator.matches(dp, d1)) {
        | IndetMatch
        | DoesNotMatch => Indet(Let(dp, d1, d2)) |> return
        | Matches(env') =>
          let* env = env |> Evaluator.evaluate_extend_env(env');
          Return.continue(~env, d2);
        }
      };

    | Filter(filter, d1) =>
      let fenv = FilterEnvironment.extends(filter, fenv);
      Return.continue(~fenv, d1);

    | FixF(f, ty, d1) =>
      let* env =
        env
        |> Evaluator.evaluate_extend_env(
             Environment.singleton((f, DHExp.FixF(f, ty, d1))),
           );
      Return.continue(~env, d1);

    | Fun(_) => BoxedValue(Closure(env, fenv, d)) |> return

    | Ap(d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2)
        | Indet(d2)
        | BoxedValue(d2) => Step(Ap(d1, d2)) |> return
        };
      | BoxedValue(TestLit(id)) =>
        let* r2 = Evaluator.evaluate_test(env, id, d2);
        switch (r2) {
        | Indet(r2) => Indet(r2) |> return
        | BoxedValue(r2) => BoxedValue(r2) |> return
        };
      | BoxedValue(Constructor(_) as d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(Ap(d1, d2)) |> return
        | Indet(d2) => Indet(Ap(d1, d2)) |> return
        | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
        };
      | BoxedValue(Closure(env, fenv, Fun(dp, _, d3, _)) as d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(Ap(d1, d2)) |> return
        | BoxedValue(_)
        | Indet(_) =>
          switch (Evaluator.matches(dp, d2)) {
          | DoesNotMatch
          | IndetMatch => Indet(Ap(d1, d2)) |> return
          | Matches(env') =>
            /* evaluate a closure: extend the closure environment with the
             * new bindings introduced by the function application. */
            let* env = env |> Evaluator.evaluate_extend_env(env');
            Return.continue(~env, ~fenv, d3);
          }
        };
      | BoxedValue(Cast(d1, Arrow(ty1, ty2), Arrow(ty1', ty2')))
      | Indet(Cast(d1, Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(Ap(d1, d2)) |> return
        | Indet(d2)
        | BoxedValue(d2) =>
          /* ap cast rule */
          Return.continue(Cast(Ap(d1, Cast(d2, ty1', ty1)), ty2, ty2'))
        };
      | BoxedValue(d1) =>
        print_endline("InvalidBoxedFun: " ++ DHExp.show(d1));
        raise(EvaluatorError.Exception(InvalidBoxedFun(d1)));
      | Indet(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(Ap(d1, d2)) |> return
        | Indet(d2)
        | BoxedValue(d2) => Indet(Ap(d1, d2)) |> return
        };
      };

    | ApBuiltin(ident, args) =>
      let* r = Evaluator.evaluate_ap_builtin(env, ident, args);
      switch (r) {
      | Indet(d) => Indet(d) |> return
      | BoxedValue(d) => Return.builtin(d)
      };

    | TestLit(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | Constructor(_) => BoxedValue(d) |> return

    | BinBoolOp(op, d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2)
        | Indet(d2)
        | BoxedValue(d2) => Step(BinBoolOp(op, d1, d2)) |> return
        };
      | BoxedValue(BoolLit(b1) as d1) =>
        switch (Evaluator.eval_bin_bool_op_short_circuit(op, b1)) {
        | Some(b3) => Return.builtin(b3)
        | None =>
          let* r2 = transition(d2);
          switch (r2) {
          | Step(d2) => Step(BinBoolOp(op, d1, d2)) |> return
          | Indet(d2) => Indet(BinBoolOp(op, d1, d2)) |> return
          | BoxedValue(BoolLit(b2)) =>
            Return.builtin(Evaluator.eval_bin_bool_op(op, b1, b2))
          | BoxedValue(d2) =>
            print_endline("InvalidBoxedBoolLit");
            raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2)));
          };
        }
      | BoxedValue(d1) =>
        print_endline("InvalidBoxedBoolLit");
        raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)));
      | Indet(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinBoolOp(op, d1, d2)) |> return
        | Indet(d2)
        | BoxedValue(d2) => Indet(BinBoolOp(op, d1, d2)) |> return
        };
      };

    | BinIntOp(op, d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2)
        | Indet(d2)
        | BoxedValue(d2) => Step(BinIntOp(op, d1, d2)) |> return
        };
      | BoxedValue(IntLit(n1) as d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinIntOp(op, d1, d2)) |> return
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
          | _ => Return.builtin(Evaluator.eval_bin_int_op(op, n1, n2))
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
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinIntOp(op, d1, d2)) |> return
        | BoxedValue(d2)
        | Indet(d2) => Indet(BinIntOp(op, d1, d2)) |> return
        };
      };

    | BinFloatOp(op, d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2)
        | Indet(d2)
        | BoxedValue(d2) => Step(BinFloatOp(op, d1, d2)) |> return
        };
      | BoxedValue(FloatLit(f1) as d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinFloatOp(op, d1, d2)) |> return
        | BoxedValue(FloatLit(f2)) =>
          Return.builtin(Evaluator.eval_bin_float_op(op, f1, f2))
        | BoxedValue(d2) =>
          print_endline("InvalidBoxedFloatLit");
          raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2)));
        | Indet(d2) => Indet(BinFloatOp(op, d1, d2)) |> return
        };
      | BoxedValue(d1) =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)));
      | Indet(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinFloatOp(op, d1, d2)) |> return
        | BoxedValue(d2)
        | Indet(d2) => Indet(BinFloatOp(op, d1, d2)) |> return
        };
      };

    | BinStringOp(op, d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2)
        | Indet(d2)
        | BoxedValue(d2) => Step(BinStringOp(op, d1, d2)) |> return
        };
      | BoxedValue(StringLit(s1) as d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinStringOp(op, d1, d2)) |> return
        | BoxedValue(StringLit(s2)) =>
          Return.builtin(Evaluator.eval_bin_string_op(op, s1, s2))
        | BoxedValue(d2) =>
          print_endline("InvalidBoxedStringLit");
          raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2)));
        | Indet(d2) => Indet(BinStringOp(op, d1, d2)) |> return
        };
      | BoxedValue(d1) =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1)));
      | Indet(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(BinStringOp(op, d1, d2)) |> return
        | BoxedValue(d2)
        | Indet(d2) => Indet(BinStringOp(op, d1, d2)) |> return
        };
      };

    | ListConcat(d1, d2) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2)
        | Indet(d2)
        | BoxedValue(d2) => Step(ListConcat(d1, d2)) |> return
        };
      | BoxedValue(d1) =>
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(ListConcat(d1, d2)) |> return
        | BoxedValue(d2') =>
          switch (d1, d2') {
          | (ListLit(u, i, ty, ds1), ListLit(_, _, _, ds2)) =>
            Return.builtin(ListLit(u, i, ty, ds1 @ ds2))
          | (Cast(d1, List(ty), List(ty')), d2)
          | (d1, Cast(d2, List(ty), List(ty'))) =>
            transition(Cast(ListConcat(d1, d2), List(ty), List(ty')))
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
        let* r2 = transition(d2);
        switch (r2) {
        | Step(d2) => Step(ListConcat(d1, d2)) |> return
        | Indet(d2)
        | BoxedValue(d2) => Indet(ListConcat(d1, d2)) |> return
        };
      };

    | Tuple(ds) =>
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
      let d' = DHExp.Tuple(ds');

      switch (tag) {
      | Step(_) => Step(d')
      | Indet(_) => Indet(d')
      | BoxedValue(_) => BoxedValue(d')
      };

    | Prj(targ, n) =>
      if (n < 0) {
        Indet(InvalidOperation(d, InvalidOperationError.InvalidProjection))
        |> return;
      } else {
        let* r = transition(targ);
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
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            )
            |> return;
          } else {
            Return.builtin(List.nth(ds, n));
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
        | Step(Cast(targ', Prod(tys), Prod(tys')) as rv)
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
            transition(Cast(Prj(targ', n), ty, ty'));
          }
        | Step(d) => Step(Prj(d, n)) |> return
        | _ => Indet(d) |> return
        };
      }

    | Cons(d1, d2) =>
      let* r1 = transition(d1);
      let* r2 = transition(d2);
      switch (r1, r2) {
      | (Step(d1), Step(d2))
      | (Step(d1), Indet(d2))
      | (Step(d1), BoxedValue(d2))
      | (Indet(d1), Step(d2))
      | (BoxedValue(d1), Step(d2)) => Step(Cons(d1, d2)) |> return
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2)) |> return
      | (BoxedValue(d1), BoxedValue(d2)) =>
        switch (d2) {
        | ListLit(u, i, ty, ds) =>
          Return.builtin(ListLit(u, i, ty, [d1, ...ds]))
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

      let d' = DHExp.ListLit(u, i, ty, ds');

      switch (tag) {
      | Step(_) => Step(d')
      | Indet(_) => Indet(d')
      | BoxedValue(_) => BoxedValue(d')
      };

    | ConsistentCase(Case(d1, rules, n)) =>
      transition_case(env, fenv, fact, d1, rules, n)

    /* Generalized closures evaluate to themselves. Only
       lambda closures are BoxedValues; other closures are all Indet. */
    | Closure(env, fenv, d) =>
      switch (d) {
      | Fun(_) => BoxedValue(Closure(env, fenv, d)) |> return
      | d => transition(~env, ~fenv, d)
      }

    /* Hole expressions */
    | InconsistentBranches(u, i, Case(d1, rules, n)) =>
      //TODO: revisit this, consider some kind of dynamic casting
      Indet(
        Closure(env, fenv, InconsistentBranches(u, i, Case(d1, rules, n))),
      )
      |> return

    | EmptyHole(u, i) =>
      Indet(Closure(env, fenv, EmptyHole(u, i))) |> return

    | NonEmptyHole(reason, u, i, d1) =>
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1') =>
        Step(Closure(env, fenv, NonEmptyHole(reason, u, i, d1'))) |> return
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
      let* r1 = transition(d1);
      switch (r1) {
      | Step(d1) => Step(Cast(d1, ty, ty')) |> return
      | BoxedValue(d1) as result =>
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
          transition(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1, ty, ty_grounded), ty_grounded, ty');
          transition(d');
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
          transition(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1, ty, ty_grounded), ty_grounded, ty');
          transition(d');
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
      let* r1 = transition(d1);
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
        fenv: FilterEnvironment.t,
        fact: FilterAction.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t) => {
    open Result;
    let* rscrut = transition(env, fenv, fact, scrut);
    switch (rscrut) {
    | BoxedValue(scrut) =>
      transition_rule(env, fenv, scrut, rules, current_rule_index)
    | Indet(scrut) =>
      eval_rule(env, fenv, fact, scrut, rules, current_rule_index)
    | Step(scrut) =>
      Step(ConsistentCase(Case(scrut, rules, current_rule_index)))
      |> Monad.return
    };
  }
  and transition_rule =
      (
        env: ClosureEnvironment.t,
        fenv: FilterEnvironment.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t) => {
    Result.(
      switch (List.nth_opt(rules, current_rule_index)) {
      | None =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        Indet(Closure(env, fenv, ConsistentCase(case))) |> Monad.return;
      | Some(Rule(dp, d)) =>
        switch (Evaluator.matches(dp, scrut)) {
        | IndetMatch =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          Indet(Closure(env, fenv, ConsistentCase(case))) |> Monad.return;
        | Matches(env') =>
          // extend environment with new bindings introduced
          let* env = env |> Evaluator.evaluate_extend_env(env');
          Step(Closure(env, fenv, d)) |> Monad.return;
        // by the rule and evaluate the expression.
        | DoesNotMatch =>
          transition_rule(env, fenv, scrut, rules, current_rule_index + 1)
        }
      }
    );
  }
  and eval_rule =
      (
        env: ClosureEnvironment.t,
        fenv: FilterEnvironment.t,
        fact: FilterAction.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t) => {
    Result.(
      switch (List.nth_opt(rules, current_rule_index)) {
      | None =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        Indet(Closure(env, fenv, ConsistentCase(case))) |> Monad.return;
      | Some(Rule(dp, d)) =>
        switch (Evaluator.matches(dp, scrut)) {
        | IndetMatch =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          Indet(Closure(env, fenv, ConsistentCase(case))) |> Monad.return;
        | Matches(env') =>
          // extend environment with new bindings introduced
          let* env = env |> Evaluator.evaluate_extend_env(env');
          transition(env, fenv, fact, d);
        // by the rule and evaluate the expression.
        | DoesNotMatch =>
          eval_rule(env, fenv, fact, scrut, rules, current_rule_index + 1)
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

    [@deriving show({with_path: false})]
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

    let return = (act: FilterAction.t, obj: EvalObj.t): t => {
      switch (act) {
      | Eval => Eval(obj)
      | Pause => Step([obj])
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
    print_endline("decomp exp = " ++ DHExp.show(exp));
    print_endline("matching flt = " ++ FilterEnvironment.show(flt));
    let act = FilterEnvironment.matches(exp, act, flt);
    print_endline("matched act = " ++ FilterAction.show(act));
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
        | (Pause, Step)
        | (Pause, Eval)
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
          switch (cat) {
          | Operator => Eval(EvalObj.mark(env, flt, Eval, exp))
          | Constructor => BoxedValue
          }
        | (Pause, BoxedValue) =>
          switch (cat) {
          | Operator => Step([EvalObj.mark(env, flt, Pause, exp)])
          | Constructor => BoxedValue
          }
        | (Eval, Indet)
        | (Pause, Indet) => Indet
        };
      };

      let wrap = f => Result.map(EvalObj.wrap(f));

      let mark = act => {
        EvalObj.mark(env, flt, act, exp)
        |> Result.return(act)
        |> Monad.return;
      };

      let merge = (cat: t, rs) => rs |> merge(cat) |> Monad.return;

      let boxed = Result.BoxedValue |> Monad.return;

      let indet = Result.Indet |> Monad.return;
    };

    let* r = Transition.transition(env, flt, Pause, exp);
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
      | Closure(_, _, Fun(_)) => Return.boxed
      | Closure(env, fenv, d1) =>
        decompose(~env, ~flt=fenv, d1)
        >>| Return.wrap(c => Closure(env, fenv, c))
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
      | Filter(flt', d1) =>
        let flt = FilterEnvironment.extends(flt', flt);
        decompose(~env, ~flt, d1) >>| Return.wrap(c => Filter(flt', c));
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
  | Closure(env, fenv, ctx) =>
    let* d = compose(ctx, d);
    Closure(env, fenv, d) |> return;

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
  | Filter(dp, ctx) =>
    let+ d = compose(ctx, d);
    Filter(dp, d);
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
  let* r = Transition.transition(obj.env, obj.flt, obj.act, obj.exp);
  let* d = compose(obj.ctx, Transition.Result.unbox(r));
  switch (r) {
  | Step(_)
  | BoxedValue(_) => EvaluatorResult.BoxedValue(d) |> return
  | Indet(_) => EvaluatorResult.Indet(d) |> return
  };
};

let evaluate_with_history = (d: DHExp.t) => {
  let rec go =
          (
            env: ClosureEnvironment.t,
            fenv: FilterEnvironment.t,
            fact: FilterAction.t,
            d: DHExp.t,
            rs: m(list(DHExp.t)),
          )
          : m(list(DHExp.t)) => {
    let* rs = rs;
    let* r = Transition.transition(env, fenv, fact, d);
    switch (r) {
    | Step(d) => go(env, fenv, fact, d, [d, ...rs] |> return)
    | BoxedValue(_) => rs |> return
    | Indet(_) => rs |> return
    };
  };
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  let fenv = FilterEnvironment.empty;
  let (_, rs) = go(env, fenv, Pause, d, [] |> return, es);
  rs;
};

let step = (obj: EvalObj.t) => {
  step(obj, EvaluatorState.init);
};

let step = Core.Memo.general(~cache_size_bound=1000, step);

let step = (obj: EvalObj.t): ProgramResult.t => {
  let (es, d) = step(obj);
  print_endline("stepped");
  d |> EvaluatorResult.show |> print_endline;
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
  print_endline("decomposing");
  let (_, rs) =
    Decompose.decompose(env, FilterEnvironment.empty, Pause, d, es);
  print_endline("decomposed");
  print_endline(
    "len = " ++ string_of_int(List.length(rs |> Decompose.Result.unbox)),
  );
  List.iter(
    r => {r |> EvalObj.show |> print_endline},
    rs |> Decompose.Result.unbox,
  );
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
    | ConsistentCase(_) => "matching"
    | BoundVar(_) => "variable lookup"
    | Sequence(_) => "sequence"
    | Let(_) => "substitution"
    | Ap(TestLit(_), _) => "evaluating a test"
    | Ap(Closure(_, _, Fun(_)), _) => "function application"
    | ApBuiltin(_) => "builtin application" // TODO[Matt]: What is a builtin Ap?
    | BinBoolOp(_) => "boolean logic"
    | BinIntOp(_) => "arithmetic"
    | BinFloatOp(_) => "arithmetic"
    | BinStringOp(_) => "string operation"
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
    | Cast(_)
    | FailedCast(_)
    | InvalidOperation(_)
    | FreeVar(_)
    | Ap(Constructor(_), _)
    | Ap(Cast(_), _)
    | Ap(_, _) => "unidentified step";

  let get_history = stepper => stepper.previous;
};

let () = {
  ignore(Transition.transition_case);
};
