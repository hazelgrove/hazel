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
    | Closure(
        [@opaque] ClosureEnvironment.t,
        [@opaque] FilterEnvironment.t,
        t,
      )
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
    env: [@opaque] ClosureEnvironment.t,
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
    [@deriving show({with_path: false})]
    type t('a) =
      | Paused('a)
      | Stepped('a)
      | Indet('a)
      | BoxedValue('a);
    let unbox =
      fun
      | Paused(d)
      | Stepped(d)
      | Indet(d)
      | BoxedValue(d) => d;
  };

  let rec transition_rule =
          (
            env: ClosureEnvironment.t,
            flt: FilterEnvironment.t,
            scrut: DHExp.t,
            rules: list(DHExp.rule),
            current_rule_index: int,
          )
          : m(Result.t(DHExp.t)) => {
    Result.(
      DHExp.(
        switch (List.nth_opt(rules, current_rule_index)) {
        | None =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          Indet(Closure(env, flt, ConsistentCase(case))) |> return;
        | Some(Rule(dp, d)) =>
          switch (Evaluator.matches(dp, scrut)) {
          | IndetMatch =>
            let case = DHExp.Case(scrut, rules, current_rule_index);
            Result.Indet(Closure(env, flt, ConsistentCase(case))) |> return;
          | Matches(env') =>
            // extend environment with new bindings introduced
            let* env = env |> Evaluator.evaluate_extend_env(env');
            Result.Stepped(Closure(env, flt, d)) |> return;
          // by the rule and evaluate the expression.
          | DoesNotMatch =>
            transition_rule(env, flt, scrut, rules, current_rule_index + 1)
          }
        }
      )
    );
  }
  and transition_case =
      (
        transition:
          (
            ~env: ClosureEnvironment.t=?,
            ~flt: FilterEnvironment.t=?,
            DHExp.t
          ) =>
          m(Result.t(DHExp.t)),
        env: ClosureEnvironment.t,
        flt: FilterEnvironment.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t(DHExp.t)) => {
    open Result;
    open DHExp;
    let* rscrut = transition(~env, ~flt, scrut);
    switch (rscrut) {
    | BoxedValue(scrut) =>
      transition_rule(env, flt, scrut, rules, current_rule_index)
    | Indet(scrut) =>
      eval_rule(transition, env, flt, scrut, rules, current_rule_index)
    | Paused(scrut) =>
      Paused(ConsistentCase(Case(scrut, rules, current_rule_index)))
      |> return
    | Stepped(scrut) =>
      Stepped(ConsistentCase(Case(scrut, rules, current_rule_index)))
      |> return
    };
  }
  and eval_rule =
      (
        transition:
          (
            ~env: ClosureEnvironment.t=?,
            ~flt: FilterEnvironment.t=?,
            DHExp.t
          ) =>
          m(Result.t(DHExp.t)),
        env: ClosureEnvironment.t,
        flt: FilterEnvironment.t,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : m(Result.t(DHExp.t)) => {
    Result.(
      DHExp.(
        switch (List.nth_opt(rules, current_rule_index)) {
        | None =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          Indet(Closure(env, flt, ConsistentCase(case))) |> return;
        | Some(Rule(dp, d)) =>
          switch (Evaluator.matches(dp, scrut)) {
          | IndetMatch =>
            let case = DHExp.Case(scrut, rules, current_rule_index);
            Indet(Closure(env, flt, ConsistentCase(case))) |> return;
          | Matches(env') =>
            // extend environment with new bindings introduced
            let* env = env |> Evaluator.evaluate_extend_env(env');
            transition(~env, d);
          // by the rule and evaluate the expression.
          | DoesNotMatch =>
            eval_rule(
              transition,
              env,
              flt,
              scrut,
              rules,
              current_rule_index + 1,
            )
          }
        }
      )
    );
  }
  and transition_test =
      (
        transition:
          (
            ~env: ClosureEnvironment.t=?,
            ~flt: FilterEnvironment.t=?,
            DHExp.t
          ) =>
          m(Result.t(DHExp.t)),
        env: ClosureEnvironment.t,
        flt: FilterEnvironment.t,
        n: KeywordID.t,
        arg: DHExp.t,
      )
      : m(Result.t(DHExp.t)) => {
    open Monad;
    let* (arg_show, arg_result) =
      switch (DHExp.strip_casts(arg)) {
      | BinBoolOp(op, arg_d1, arg_d2) =>
        let mk_op = (arg_d1, arg_d2) => DHExp.BinBoolOp(op, arg_d1, arg_d2);
        transition_test_eq(transition, env, flt, mk_op, arg_d1, arg_d2);
      | BinIntOp(op, arg_d1, arg_d2) =>
        let mk_op = (arg_d1, arg_d2) => DHExp.BinIntOp(op, arg_d1, arg_d2);
        transition_test_eq(transition, env, flt, mk_op, arg_d1, arg_d2);
      | BinFloatOp(op, arg_d1, arg_d2) =>
        let mk_op = (arg_d1, arg_d2) => DHExp.BinFloatOp(op, arg_d1, arg_d2);
        transition_test_eq(transition, env, flt, mk_op, arg_d1, arg_d2);

      | Ap(fn, Tuple(args)) =>
        let* args_d: list(Result.t(DHExp.t)) =
          args |> List.map(transition) |> sequence;
        let arg_show = DHExp.Ap(fn, Tuple(List.map(Result.unbox, args_d)));
        let* arg_result = transition(arg_show);
        (arg_show, arg_result) |> return;

      | Ap(Ap(arg_d1, arg_d2), arg_d3) =>
        let* arg_d1 = transition(arg_d1);
        let* arg_d2 = transition(arg_d2);
        let* arg_d3 = transition(arg_d3);
        let arg_show =
          DHExp.Ap(
            Ap(Result.unbox(arg_d1), Result.unbox(arg_d2)),
            Result.unbox(arg_d3),
          );
        let* arg_result = transition(arg_show);
        (arg_show, arg_result) |> return;

      | Ap(arg_d1, arg_d2) =>
        let mk = (arg_d1, arg_d2) => DHExp.Ap(arg_d1, arg_d2);
        transition_test_eq(transition, env, flt, mk, arg_d1, arg_d2);

      | _ =>
        let* arg = transition(arg);
        (Result.unbox(arg), arg) |> return;
      };

    let test_status: TestStatus.t =
      switch (arg_result) {
      | BoxedValue(BoolLit(true)) => Pass
      | BoxedValue(BoolLit(false)) => Fail
      | _ => Indet
      };

    let* _ = add_test(n, (arg_show, test_status));
    let r: Result.t(DHExp.t) =
      switch (arg_result) {
      | Paused(arg) => Paused(Ap(TestLit(n), arg))
      | Stepped(arg) => Stepped(Ap(TestLit(n), arg))
      | BoxedValue(BoolLit(_)) => BoxedValue(Tuple([]))
      | BoxedValue(arg)
      | Indet(arg) => Indet(Ap(TestLit(n), arg))
      };
    r |> return;
  }
  and transition_test_eq =
      (
        transition:
          (
            ~env: ClosureEnvironment.t=?,
            ~flt: FilterEnvironment.t=?,
            DHExp.t
          ) =>
          m(Result.t(DHExp.t)),
        _env: ClosureEnvironment.t,
        _flt: FilterEnvironment.t,
        mk_arg_op: (DHExp.t, DHExp.t) => DHExp.t,
        arg_d1: DHExp.t,
        arg_d2: DHExp.t,
      )
      : m((DHExp.t, Result.t(DHExp.t))) => {
    let* arg_d1 = transition(arg_d1);
    let* arg_d2 = transition(arg_d2);

    let arg_show = mk_arg_op(Result.unbox(arg_d1), Result.unbox(arg_d2));
    let* arg_result = transition(arg_show);

    (arg_show, arg_result) |> return;
  };

  let transition =
      (
        transition:
          (
            ~env: ClosureEnvironment.t=?,
            ~flt: FilterEnvironment.t=?,
            DHExp.t
          ) =>
          m(Result.t(DHExp.t)),
        env: ClosureEnvironment.t,
        flt: FilterEnvironment.t,
        d: DHExp.t,
      )
      : m(Result.t(DHExp.t)) => {
    Evaluator.(
      Result.(
        DHExp.
          // let (and|) = (f, d) => {
          //   let* f = f;
          //   switch (f) {
          //   | `BoxedValue(f, `Some(t)) =>
          //     let* r = transition(d);
          //     switch (r) {
          //     | BoxedValue(d) =>
          //       `BoxedValue((f(d), `Some((t, `BoxedValue(d))))) |> return
          //     | Indet(d) => `Indet((f(d), `Some((t, `Indet(d))))) |> return
          //     | Expr(d) => `Expr((f(d), `None)) |> return
          //     }
          //   | `Indet(f, `None) =>
          //     let* r = transition(d);
          //     switch (r) {
          //     | BoxedValue(d)
          //     | Indet(d) => `Indet((f(d), `None)) |> return
          //     | Expr(d) => `Expr((f(d), `None)) |> return
          //     }
          //   | `Expr(f, `None) => `Expr((f(d), `None)) |> return
          //   }
          // };
          // let (and&) = (f, d) => {
          //   let* f = f;
          //   switch (f) {
          //   | `BoxedValue(f, `Some(t)) =>
          //     let* r = transition(d);
          //     switch (r) {
          //     | BoxedValue(d) =>
          //       `BoxedValue((f(d), `Some((t, `BoxedValue(d))))) |> return
          //     | Indet(d) => `Indet((f(d), `None)) |> return
          //     | Expr(d) => `Expr((f(d), `None)) |> return
          //     };
          //   | `Indet(f, `Some(t)) =>
          //     let* r = transition(d);
          //     switch (r) {
          //     | BoxedValue(d) =>
          //       `Indet((f(d), `Some((t, `BoxedValue(d))))) |> return
          //     | Indet(d) => `Indet((f(d), `None)) |> return
          //     | Expr(d) => `Expr((f(d), `None)) |> return
          //     };
          //   | `Indet(f, `None) =>
          //     let* r = transition(d);
          //     switch (r) {
          //     | BoxedValue(d)
          //     | Indet(d) => `Indet((f(d), `None)) |> return
          //     | Expr(d) => `Expr((f(d), `None)) |> return
          //     };
          //   | `Expr(f, _) => `Expr((f(d), `None)) |> return
          //   };
          // };
          // let (let@) = (f, k) => {
          //   let* f = f;
          //   switch (f) {
          //   | `Expr(d, _) => StepperResult.Expr(d) |> return
          //   | `Indet(d, `None) => StepperResult.Indet(d) |> return
          //   | `Indet(_, `Some(t))
          //   | `BoxedValue(_, `Some(t)) => k(t)
          //   };
          // };
          /* TODO: Investigate */
          /* Increment number of evaluation steps (calls to `evaluate`). */
          (
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
              Stepped(d) |> return;

            | Sequence(d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(Sequence(d1, d2)) |> return
              | Paused(d1) => Paused(Sequence(d1, d2)) |> return
              | BoxedValue(d1)
              /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
               * if evaluated d1 is indet. */
              | Indet(d1) =>
                d1 |> ignore;
                /* let* r2 = step(env, d2, opt); */
                /* switch (r2) { */
                /* | BoxedValue(d2) */
                /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
                /* }; */
                Stepped(d2) |> return;
              };

            | Let(dp, d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(Let(dp, d1, d2)) |> return
              | Paused(d1) => Paused(Let(dp, d1, d2)) |> return
              | BoxedValue(d1)
              | Indet(d1) =>
                switch (Evaluator.matches(dp, d1)) {
                | IndetMatch
                | DoesNotMatch => Indet(Let(dp, d1, d2)) |> return
                | Matches(env') =>
                  let* env = env |> Evaluator.evaluate_extend_env(env');
                  Stepped(Closure(env, flt, d2)) |> return;
                }
              };

            | Filter(flt', d1) =>
              let flt = flt |> FilterEnvironment.extends(flt');
              Stepped(Closure(env, flt, d1)) |> return;

            | FixF(f, ty, d1) =>
              let env' = Environment.singleton((f, DHExp.FixF(f, ty, d1)));
              let* env = env |> Evaluator.evaluate_extend_env(env');
              Stepped(Closure(env, flt, d1)) |> return;

            | Fun(_) => Stepped(Closure(env, flt, d)) |> return

            | Ap(d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(Ap(d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(Ap(d1, d2)) |> return
                | Stepped(d2) => Stepped(Ap(d1, d2)) |> return
                | Indet(d2) => Paused(Ap(d1, d2)) |> return
                | BoxedValue(d2) => Paused(Ap(d1, d2)) |> return
                };
              | BoxedValue(TestLit(id)) =>
                let* r2 = transition_test(transition, env, flt, id, d2);
                switch (r2) {
                | Stepped(r2) => Stepped(r2) |> return
                | Paused(r2) => Paused(r2) |> return
                | Indet(r2) => Indet(r2) |> return
                | BoxedValue(r2) => BoxedValue(r2) |> return
                };
              | BoxedValue(Constructor(_) as d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(Ap(d1, d2)) |> return
                | Stepped(d2) => Stepped(Ap(d1, d2)) |> return
                | Indet(d2) => Indet(Ap(d1, d2)) |> return
                | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
                };
              | BoxedValue(Closure(env, flt, Fun(dp, _, d3, _)) as d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(Ap(d1, d2)) |> return
                | Stepped(d2) => Stepped(Ap(d1, d2)) |> return
                | BoxedValue(d2)
                | Indet(d2) =>
                  switch (Evaluator.matches(dp, d2)) {
                  | DoesNotMatch
                  | IndetMatch => Indet(Ap(d1, d2)) |> return
                  | Matches(env') =>
                    /* evaluate a closure: extend the closure environment with the
                     * new bindings introduced by the function application. */
                    let* env = env |> Evaluator.evaluate_extend_env(env');
                    Stepped(Closure(env, flt, d3)) |> return;
                  }
                };
              | BoxedValue(Cast(d1, Arrow(ty1, ty2), Arrow(ty1', ty2')))
              | Indet(Cast(d1, Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(Ap(d1, d2)) |> return
                | Stepped(d2) => Stepped(Ap(d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) =>
                  /* ap cast rule */
                  Stepped(Cast(Ap(d1, Cast(d2, ty1', ty1)), ty2, ty2'))
                  |> return
                };
              | BoxedValue(d1) =>
                print_endline("InvalidBoxedFun: " ++ DHExp.show(d1));
                raise(EvaluatorError.Exception(InvalidBoxedFun(d1)));
              | Indet(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(Ap(d1, d2)) |> return
                | Stepped(d2) => Stepped(Ap(d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Indet(Ap(d1, d2)) |> return
                };
              };

            | ApBuiltin(ident, args) =>
              let* r = Evaluator.evaluate_ap_builtin(env, ident, args);
              switch (r) {
              | Indet(d) => Indet(d) |> return
              | BoxedValue(d) => Stepped(d) |> return
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
              | Stepped(d1) => Stepped(BinBoolOp(op, d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinBoolOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinBoolOp(op, d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Paused(BinBoolOp(op, d1, d2)) |> return
                };
              | BoxedValue(BoolLit(b1) as d1) =>
                switch (Evaluator.eval_bin_bool_op_short_circuit(op, b1)) {
                | Some(b3) => Stepped(b3) |> return
                | None =>
                  let* r2 = transition(d2);
                  switch (r2) {
                  | Paused(d2) => Paused(BinBoolOp(op, d1, d2)) |> return
                  | Stepped(d2) => Stepped(BinBoolOp(op, d1, d2)) |> return
                  | BoxedValue(BoolLit(b2)) =>
                    Stepped(Evaluator.eval_bin_bool_op(op, b1, b2)) |> return
                  | BoxedValue(d2) =>
                    print_endline("InvalidBoxedBoolLit");
                    raise(
                      EvaluatorError.Exception(InvalidBoxedBoolLit(d2)),
                    );
                  | Indet(d2) => Indet(BinBoolOp(op, d1, d2)) |> return
                  };
                }
              | BoxedValue(d1) =>
                print_endline("InvalidBoxedBoolLit");
                raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)));
              | Indet(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinBoolOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinBoolOp(op, d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Indet(BinBoolOp(op, d1, d2)) |> return
                };
              };

            | BinIntOp(op, d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(BinIntOp(op, d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinIntOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinIntOp(op, d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Paused(BinIntOp(op, d1, d2)) |> return
                };
              | BoxedValue(IntLit(n1) as d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinIntOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinIntOp(op, d1, d2)) |> return
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
                  | _ =>
                    Stepped(Evaluator.eval_bin_int_op(op, n1, n2)) |> return
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
                | Paused(d2) => Paused(BinIntOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinIntOp(op, d1, d2)) |> return
                | BoxedValue(d2)
                | Indet(d2) => Indet(BinIntOp(op, d1, d2)) |> return
                };
              };

            | BinFloatOp(op, d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(BinFloatOp(op, d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinFloatOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinFloatOp(op, d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Stepped(BinFloatOp(op, d1, d2)) |> return
                };
              | BoxedValue(FloatLit(f1) as d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinFloatOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinFloatOp(op, d1, d2)) |> return
                | BoxedValue(FloatLit(f2)) =>
                  Stepped(Evaluator.eval_bin_float_op(op, f1, f2)) |> return
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
                | Paused(d2) => Paused(BinFloatOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinFloatOp(op, d1, d2)) |> return
                | BoxedValue(d2)
                | Indet(d2) => Indet(BinFloatOp(op, d1, d2)) |> return
                };
              };

            | BinStringOp(op, d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(BinStringOp(op, d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinStringOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinStringOp(op, d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Paused(BinStringOp(op, d1, d2)) |> return
                };
              | BoxedValue(StringLit(s1) as d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinStringOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinStringOp(op, d1, d2)) |> return
                | BoxedValue(StringLit(s2)) =>
                  Stepped(Evaluator.eval_bin_string_op(op, s1, s2)) |> return
                | BoxedValue(d2) =>
                  print_endline("InvalidBoxedStringLit");
                  raise(
                    EvaluatorError.Exception(InvalidBoxedStringLit(d2)),
                  );
                | Indet(d2) => Indet(BinStringOp(op, d1, d2)) |> return
                };
              | BoxedValue(d1) =>
                print_endline("InvalidBoxedStringLit");
                raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1)));
              | Indet(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(BinStringOp(op, d1, d2)) |> return
                | Stepped(d2) => Stepped(BinStringOp(op, d1, d2)) |> return
                | BoxedValue(d2)
                | Indet(d2) => Indet(BinStringOp(op, d1, d2)) |> return
                };
              };

            | ListConcat(d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(ListConcat(d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(ListConcat(d1, d2)) |> return
                | Stepped(d2) => Stepped(ListConcat(d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Paused(ListConcat(d1, d2)) |> return
                };
              | BoxedValue(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(ListConcat(d1, d2)) |> return
                | Stepped(d2) => Stepped(ListConcat(d1, d2)) |> return
                | BoxedValue(d2') =>
                  switch (d1, d2') {
                  | (ListLit(u, i, ty, ds1), ListLit(_, _, _, ds2)) =>
                    Stepped(ListLit(u, i, ty, ds1 @ ds2)) |> return
                  | (Cast(d1, List(ty), List(ty')), d2)
                  | (d1, Cast(d2, List(ty), List(ty'))) =>
                    Stepped(Cast(ListConcat(d1, d2), List(ty), List(ty')))
                    |> return
                  | (ListLit(_), _) =>
                    print_endline("InvalidBoxedListLit: " ++ DHExp.show(d2));
                    raise(
                      EvaluatorError.Exception(InvalidBoxedListLit(d2)),
                    );
                  | _ =>
                    print_endline("InvalidBoxedListLit: " ++ DHExp.show(d1));
                    raise(
                      EvaluatorError.Exception(InvalidBoxedListLit(d1)),
                    );
                  }
                | Indet(d2) => Indet(ListConcat(d1, d2)) |> return
                };
              | Indet(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Paused(d2) => Paused(ListConcat(d1, d2)) |> return
                | Stepped(d2) => Stepped(ListConcat(d1, d2)) |> return
                | Indet(d2)
                | BoxedValue(d2) => Indet(ListConcat(d1, d2)) |> return
                };
              };

            | Tuple(ds) =>
              let* ds =
                BoxedValue([])
                |> return
                |> List.fold_right(
                     (d, dst) => {
                       let* dst = dst;
                       switch (dst) {
                       | Stepped(dst) => Stepped([d, ...dst]) |> return
                       | Paused(dst) =>
                         let* r = transition(d);
                         switch (r) {
                         | Stepped(d) => Stepped([d, ...dst]) |> return
                         | Paused(d)
                         | Indet(d)
                         | BoxedValue(d) => Paused([d, ...dst]) |> return
                         };
                       | Indet(dst) =>
                         let* r = transition(d);
                         switch (r) {
                         | Stepped(d) => Stepped([d, ...dst]) |> return
                         | Paused(d) => Paused([d, ...dst]) |> return
                         | Indet(d)
                         | BoxedValue(d) => Indet([d, ...dst]) |> return
                         };
                       | BoxedValue(dst) =>
                         let* r = transition(d);
                         switch (r) {
                         | Stepped(d) => Stepped([d, ...dst]) |> return
                         | Paused(d) => Paused([d, ...dst]) |> return
                         | Indet(d) => Indet([d, ...dst]) |> return
                         | BoxedValue(d) => BoxedValue([d, ...dst]) |> return
                         };
                       };
                     },
                     ds,
                   );

              switch (ds) {
              | Stepped(ds) => Stepped(DHExp.Tuple(ds)) |> return
              | Paused(ds) => Paused(DHExp.Tuple(ds)) |> return
              | Indet(ds) => Indet(DHExp.Tuple(ds)) |> return
              | BoxedValue(ds) => BoxedValue(DHExp.Tuple(ds)) |> return
              };

            | Prj(d1, n) =>
              let invalid_projection = d =>
                InvalidOperation(d, InvalidOperationError.InvalidProjection);
              if (n < 0) {
                Stepped(Prj(d1, n) |> invalid_projection) |> return;
              } else {
                let* r = transition(d1);
                switch (r) {
                | Stepped(d1) => Stepped(Prj(d1, n)) |> return
                | Paused(d1) => Paused(Prj(d1, n)) |> return
                | BoxedValue(Tuple(ds) as d)
                | Indet(Closure(_, _, Tuple(ds)) as d)
                | Indet(Tuple(ds) as d) when n >= List.length(ds) =>
                  Stepped(d |> invalid_projection) |> return
                | BoxedValue(Tuple(ds))
                | Indet(Closure(_, _, Tuple(ds)))
                | Indet(Tuple(ds)) => Stepped(List.nth(ds, n)) |> return
                | BoxedValue(Cast(_, Prod(tys), Prod(_)) as rv)
                | Indet(Cast(_, Prod(tys), Prod(_)) as rv)
                    when n >= List.length(tys) =>
                  Indet(rv |> invalid_projection) |> return
                | BoxedValue(Cast(d, Prod(tys), Prod(tys')))
                | Indet(Cast(d, Prod(tys), Prod(tys'))) =>
                  let ty = List.nth(tys, n);
                  let ty' = List.nth(tys', n);
                  Stepped(Cast(Prj(d, n), ty, ty')) |> return;
                | BoxedValue(d)
                | Indet(d) => Indet(d) |> return
                };
              };

            | Cons(d1, d2) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(Cons(d1, d2)) |> return
              | Paused(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Stepped(d2) => Stepped(Cons(d1, d2)) |> return
                | Paused(d2)
                | Indet(d2)
                | BoxedValue(d2) => Paused(Cons(d1, d2)) |> return
                };
              | Indet(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Stepped(d2) => Stepped(Cons(d1, d2)) |> return
                | Paused(d2) => Paused(Cons(d1, d2)) |> return
                | Indet(ListLit(u, i, ty, ds))
                | BoxedValue(ListLit(u, i, ty, ds)) =>
                  Indet(ListLit(u, i, ty, [d1, ...ds])) |> return
                | Indet(d2)
                | BoxedValue(d2) => Indet(Cons(d1, d2)) |> return
                };
              | BoxedValue(d1) =>
                let* r2 = transition(d2);
                switch (r2) {
                | Stepped(d2) => Stepped(Cons(d1, d2)) |> return
                | Paused(d2) => Paused(Cons(d1, d2)) |> return
                | Indet(ListLit(u, i, ty, ds)) =>
                  Indet(ListLit(u, i, ty, [d1, ...ds])) |> return
                | BoxedValue(ListLit(u, i, ty, ds)) =>
                  Stepped(ListLit(u, i, ty, [d1, ...ds])) |> return
                | BoxedValue(Cons(_))
                | BoxedValue(Cast(ListLit(_), List(_), List(_))) =>
                  BoxedValue(Cons(d1, d2)) |> return
                | Indet(Cons(_))
                | Indet(Cast(ListLit(_), List(_), List(_))) =>
                  Indet(Cons(d1, d2)) |> return
                | _ =>
                  print_endline("InvalidBoxedListLit");
                  raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
                };
              };

            | ListLit(u, i, ty, ds) =>
              let* ds =
                BoxedValue([])
                |> return
                |> List.fold_right(
                     (d, dst) => {
                       let* dst = dst;
                       switch (dst) {
                       | Stepped(dst) => Stepped([d, ...dst]) |> return
                       | Paused(dst) =>
                         let* r = transition(d);
                         switch (r) {
                         | Stepped(d) => Stepped([d, ...dst]) |> return
                         | Paused(d)
                         | Indet(d)
                         | BoxedValue(d) => Paused([d, ...dst]) |> return
                         };
                       | Indet(dst) =>
                         let* r = transition(d);
                         switch (r) {
                         | Stepped(d) => Stepped([d, ...dst]) |> return
                         | Paused(d) => Paused([d, ...dst]) |> return
                         | Indet(d)
                         | BoxedValue(d) => Indet([d, ...dst]) |> return
                         };
                       | BoxedValue(dst) =>
                         let* r = transition(d);
                         switch (r) {
                         | Stepped(d) => Stepped([d, ...dst]) |> return
                         | Paused(d) => Paused([d, ...dst]) |> return
                         | Indet(d) => Indet([d, ...dst]) |> return
                         | BoxedValue(d) => BoxedValue([d, ...dst]) |> return
                         };
                       };
                     },
                     ds,
                   );

              switch (ds) {
              | Stepped(ds) => Stepped(DHExp.ListLit(u, i, ty, ds)) |> return
              | Paused(ds) => Paused(DHExp.ListLit(u, i, ty, ds)) |> return
              | Indet(ds) => Indet(DHExp.ListLit(u, i, ty, ds)) |> return
              | BoxedValue(ds) =>
                BoxedValue(DHExp.ListLit(u, i, ty, ds)) |> return
              };

            | ConsistentCase(Case(d1, rules, n)) =>
              transition_case(transition, env, flt, d1, rules, n)

            /* Generalized closures evaluate to themselves. Only
               lambda closures are BoxedValues; other closures are all Indet. */
            | Closure(env, flt, d) =>
              switch (d) {
              | Fun(_) => BoxedValue(Closure(env, flt, d)) |> return
              | d =>
                let* r = transition(~env, ~flt, d);
                switch (r) {
                | Stepped(d) => Stepped(Closure(env, flt, d)) |> return
                | Paused(d) => Paused(Closure(env, flt, d)) |> return
                | Indet(d) => Indet(d) |> return
                | BoxedValue(d) => BoxedValue(d) |> return
                };
              }

            /* Hole expressions */
            | InconsistentBranches(u, i, Case(d1, rules, n)) =>
              //TODO: revisit this, consider some kind of dynamic casting
              Indet(
                Closure(
                  env,
                  flt,
                  InconsistentBranches(u, i, Case(d1, rules, n)),
                ),
              )
              |> return

            | EmptyHole(u, i) =>
              Indet(Closure(env, flt, EmptyHole(u, i))) |> return

            | NonEmptyHole(reason, u, i, d1) =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) =>
                Stepped(Closure(env, flt, NonEmptyHole(reason, u, i, d1)))
                |> return
              | Paused(d1) =>
                Paused(Closure(env, flt, NonEmptyHole(reason, u, i, d1)))
                |> return
              | BoxedValue(d1)
              | Indet(d1) =>
                Indet(Closure(env, flt, NonEmptyHole(reason, u, i, d1)))
                |> return
              };

            | FreeVar(u, i, x) =>
              Indet(Closure(env, flt, FreeVar(u, i, x))) |> return

            | ExpandingKeyword(u, i, kw) =>
              Indet(Closure(env, flt, ExpandingKeyword(u, i, kw))) |> return

            | InvalidText(u, i, text) =>
              Indet(Closure(env, flt, InvalidText(u, i, text))) |> return

            /* Cast calculus */
            | Cast(d1, ty, ty') =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(Cast(d1, ty, ty')) |> return
              | Paused(d1) => Paused(Cast(d1, ty, ty')) |> return
              | BoxedValue(d1) =>
                switch (
                  Evaluator.ground_cases_of(ty),
                  Evaluator.ground_cases_of(ty'),
                ) {
                | (Hole, Hole) => BoxedValue(d1) |> return
                | (Ground, Ground) =>
                  /* if two types are ground and consistent, then they are eq */
                  BoxedValue(d1) |> return
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
                  let d1 =
                    DHExp.Cast(
                      Cast(d1, ty, ty'_grounded),
                      ty'_grounded,
                      ty',
                    );
                  Stepped(d1) |> return;
                | (NotGroundOrHole(ty_grounded), Hole) =>
                  /* ITGround rule */
                  let d1 =
                    DHExp.Cast(Cast(d1, ty, ty_grounded), ty_grounded, ty');
                  Stepped(d1) |> return;
                | (Ground, NotGroundOrHole(_))
                | (NotGroundOrHole(_), Ground) =>
                  /* can't do anything when casting between diseq, non-hole types */
                  BoxedValue(Cast(d1, ty, ty')) |> return
                | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
                  /* they might be eq in this case, so remove cast if so */
                  if (Typ.eq(ty, ty')) {
                    BoxedValue(d1) |> return;
                  } else {
                    BoxedValue(Cast(d1, ty, ty')) |> return;
                  }
                }
              | Indet(d1) =>
                switch (
                  Evaluator.ground_cases_of(ty),
                  Evaluator.ground_cases_of(ty'),
                ) {
                | (Hole, Hole) => Indet(d1) |> return
                | (Ground, Ground) =>
                  /* if two types are ground and consistent, then they are eq */
                  Indet(d1) |> return
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
                  let d1 =
                    DHExp.Cast(
                      Cast(d1, ty, ty'_grounded),
                      ty'_grounded,
                      ty',
                    );
                  Stepped(d1) |> return;
                | (NotGroundOrHole(ty_grounded), Hole) =>
                  /* ITGround rule */
                  let d1 =
                    DHExp.Cast(Cast(d1, ty, ty_grounded), ty_grounded, ty');
                  Stepped(d1) |> return;
                | (Ground, NotGroundOrHole(_))
                | (NotGroundOrHole(_), Ground) =>
                  /* can't do anything when casting between diseq, non-hole types */
                  Indet(Cast(d1, ty, ty')) |> return
                | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
                  /* it might be eq in this case, so remove cast if so */
                  if (Typ.eq(ty, ty')) {
                    Indet(d1) |> return;
                  } else {
                    Indet(Cast(d1, ty, ty')) |> return;
                  }
                }
              };

            | FailedCast(d1, ty, ty') =>
              let* r1 = transition(d1);
              switch (r1) {
              | Stepped(d1) => Stepped(FailedCast(d1, ty, ty')) |> return
              | Paused(d1) => Paused(FailedCast(d1, ty, ty')) |> return
              | BoxedValue(d1)
              | Indet(d1) => Indet(FailedCast(d1, ty, ty')) |> return
              };

            | InvalidOperation(d, err) =>
              Indet(InvalidOperation(d, err)) |> return
            }
          )
      )
    );
  };

  let rec step =
          (env: ClosureEnvironment.t, flt: FilterEnvironment.t, d: DHExp.t)
          : Monad.t(Result.t(DHExp.t)) => {
    let fixed = (~env=env, ~flt=flt, d) => step(env, flt, d);
    transition(fixed, env, flt, d);
  };

  let rec walk =
          (env: ClosureEnvironment.t, flt: FilterEnvironment.t, d: DHExp.t)
          : Monad.t(Result.t(DHExp.t)) => {
    open Result;
    let act = flt |> FilterEnvironment.matches(d);
    let fixed = (~env=env, ~flt=flt, d) => walk(env, flt, d);
    let+ r = transition(fixed, env, flt, d);
    switch (act) {
    | Some(Pause) =>
      switch (r) {
      | Paused(d) => Paused(d)
      | Stepped(_) => Paused(d)
      | Indet(d) => Indet(d)
      | BoxedValue(d) => BoxedValue(d)
      }
    | Some(Eval)
    | None => r
    };
  };

  let rec eval =
          (env: ClosureEnvironment.t, flt: FilterEnvironment.t, d: DHExp.t)
          : Monad.t(Result.t(DHExp.t)) => {
    open Result;
    let* r = walk(env, flt, d);
    switch (r) {
    | Paused(d) => Paused(d) |> return
    | Stepped(d) =>
      let* () = take_step;
      eval(env, flt, d);
    | BoxedValue(d) => BoxedValue(d) |> return
    | Indet(d) => Indet(d) |> return
    };
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
    print_endline("======== decompose ========");
    print_endline("exp = " ++ DHExp.show(exp));
    let decompose = (~env=env, ~flt=flt, ~act=act, exp: DHExp.t) => {
      decompose(env, flt, act, exp);
    };
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

      let mark = (act: FilterAction.t) => {
        EvalObj.mark(env, flt, act, exp)
        |> Result.return(act)
        |> Monad.return;
      };

      let merge = (cat: t, rs) => rs |> merge(cat) |> Monad.return;

      let boxed = Result.BoxedValue |> Monad.return;

      let indet = Result.Indet |> Monad.return;
    };

    let* () = 0 |> put_step;
    let* r = Transition.eval(env, flt, exp);
    let* s = get_step;
    print_endline("rexp = " ++ Transition.Result.show(DHExp.pp, r));
    switch (r) {
    | BoxedValue(_) when s == 0 => Return.boxed
    | BoxedValue(_) => Return.mark(Eval)
    | Indet(_) when s == 0 => Return.indet
    | Indet(_) => Return.mark(Eval)
    | Stepped(_)
    | Paused(_) =>
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
      | Closure(env, flt, d1) =>
        decompose(~env, ~flt, d1) >>| Return.wrap(c => Closure(env, flt, c))
      | Cast(d1, ty, ty') =>
        decompose(~env, ~flt, d1) >>| Return.wrap(c => Cast(c, ty, ty'))
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
      | Filter(_) => Return.mark(act)
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
  let* r =
    switch (obj.act) {
    | Pause => Transition.step(obj.env, obj.flt, obj.exp)
    | Eval => Transition.eval(obj.env, obj.flt, obj.exp)
    };
  let* d = compose(obj.ctx, Transition.Result.unbox(r));
  switch (r) {
  | Paused(_)
  | Stepped(_)
  | BoxedValue(_) => EvaluatorResult.BoxedValue(d) |> return
  | Indet(_) => EvaluatorResult.Indet(d) |> return
  };
};

let evaluate_with_history = (d: DHExp.t) => {
  let rec go =
          (
            env: ClosureEnvironment.t,
            fenv: FilterEnvironment.t,
            d: DHExp.t,
            rs: m(list(DHExp.t)),
          )
          : m(list(DHExp.t)) => {
    let* rs = rs;
    let* r = Transition.step(env, fenv, d);
    switch (r) {
    | Stepped(d)
    | Paused(d) => go(env, fenv, d, [d, ...rs] |> return)
    | BoxedValue(_) => rs |> return
    | Indet(_) => rs |> return
    };
  };
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  let fenv = FilterEnvironment.empty;
  let (_, rs) = go(env, fenv, d, [] |> return, es);
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
  print_endline("======== decompose ========");
  print_endline("exp = " ++ DHExp.show(d));
  let (_, rs) =
    Decompose.decompose(env, FilterEnvironment.empty, Pause, d, es);
  print_endline("rs = " ++ Decompose.Result.show(rs));
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

  let mk = d => {current: d, previous: [], next: decompose(d)};

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
