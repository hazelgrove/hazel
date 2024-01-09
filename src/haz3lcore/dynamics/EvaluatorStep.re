open Sexplib.Std;
open Transition;

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ctx: EvalCtx.t,
    apply: unit => DHExp.t,
    undo: DHExp.t,
    knd: step_kind,
  };

  let mk = (ctx, apply, undo, knd) => {ctx, apply, undo, knd};

  let get_ctx = (obj: t): EvalCtx.t => obj.ctx;
  let get_exp = (obj: t): DHExp.t => obj.apply();
  let get_kind = (obj: t): step_kind => obj.knd;

  let rec unwrap = (obj: t, sel: EvalCtx.cls): option(t) => {
    switch (sel, obj.ctx) {
    | (Mark, _) =>
      print_endline(
        "Mark does not match with "
        ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(obj.ctx)),
      );
      raise(EvaluatorError.Exception(StepDoesNotMatch));
    | (_, Mark) => None
    | (BoundVar, c)
    | (NonEmptyHole, NonEmptyHole(_, _, _, c))
    | (Closure, Closure(_, c))
    | (Filter, Filter(_, c))
    | (Sequence1, Sequence1(c, _))
    | (Sequence2, Sequence2(_, c))
    | (Let1, Let1(_, c, _))
    | (Let2, Let2(_, _, c))
    | (Fun, Fun(_, _, c, _))
    | (FixF, FixF(_, _, c))
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
    | (ListLit(n), ListLit(_, _, _, c, (ld, _)))
    | (Tuple(n), Tuple(c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (ConsistentCaseRule(n), ConsistentCaseRule(_, _, c, (ld, _), _))
    | (
        InconsistentBranchesRule(n),
        InconsistentBranchesRule(_, _, _, _, c, (ld, _), _),
      ) =>
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
    | (FailedCastCast, FailedCast(Cast(c, _, _), _, _))
    | (FailedCast, FailedCast(c, _, _)) => Some({...obj, ctx: c})
    | (Ap1, Ap2(_, _))
    | (Ap2, Ap1(_, _))
    | (Let1, Let2(_))
    | (Let2, Let1(_))
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

module Decompose = {
  module Result = {
    type t =
      | Indet
      | BoxedValue
      | Step(list(EvalObj.t));

    let unbox = (r: t): list(EvalObj.t) => {
      switch (r) {
      | Indet
      | BoxedValue => []
      | Step(objs) => objs
      };
    };
  };

  // TODO[Matt]: Add Skip/Step back to this

  module DecomposeEVMode: {
    include
      EV_MODE with
        type result = Result.t and type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t); // TODO[Matt]: Make sure this gets passed around correctly
    type requirement('a) = (Result.t, 'a);
    type requirements('a, 'b) = ('b, Result.t, ClosureEnvironment.t, 'a);
    type result = Result.t;

    let req_value = (cont, wr, d) => {
      switch (cont(d)) {
      | Result.Indet => (Result.Indet, d)
      | Result.BoxedValue => (Result.BoxedValue, d)
      | Result.Step(objs) => (
          Result.Step(List.map(EvalObj.wrap(wr), objs)),
          d,
        )
      };
    };

    let (&&): (Result.t, Result.t) => Result.t =
      (u, v) =>
        switch (u, v) {
        | (Step(ss1), Step(ss2)) => Step(ss1 @ ss2)
        | (Step(ss), _)
        | (_, Step(ss)) => Step(ss)
        | (Indet, BoxedValue)
        | (BoxedValue, Indet)
        | (Indet, Indet) => Indet
        | (BoxedValue, BoxedValue) => BoxedValue
        };

    let rec req_all_value' = (cont, wr, ds') =>
      fun
      | [] => (Result.BoxedValue, [])
      | [d, ...ds] => {
          let (r1, v) = req_value(cont, wr(_, (ds', ds)), d);
          let (r2, vs) = req_all_value'(cont, wr, [d, ...ds'], ds);
          (r1 && r2, [v, ...vs]);
        };
    let req_all_value = (cont, wr, ds) => {
      req_all_value'(cont, wr, [], ds);
    };

    let req_final = (cont, wr, d) => {
      (
        switch (cont(d)) {
        | Result.Indet => Result.BoxedValue
        | Result.BoxedValue => Result.BoxedValue
        | Result.Step(objs) =>
          Result.Step(List.map(EvalObj.wrap(wr), objs))
        },
        d,
      );
    };

    let rec req_all_final' = (cont, wr, ds') =>
      fun
      | [] => (Result.BoxedValue, [])
      | [d, ...ds] => {
          let (r1, v) = req_final(cont, wr(_, (ds', ds)), d);
          let (r2, vs) = req_all_final'(cont, wr, [d, ...ds'], ds);
          (r1 && r2, [v, ...vs]);
        };

    let req_all_final = (cont, wr, ds) => {
      req_all_final'(cont, wr, [], ds);
    };

    let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
      (rq, rl) =>
        switch (rq) {
        | (_, Result.Indet, _, _) => Result.Indet
        | (undo, Result.BoxedValue, _, v) =>
          switch (rl(v)) {
          | Constructor => Result.BoxedValue
          | Indet => Result.Indet
          | Step(s) => Result.Step([EvalObj.mk(Mark, s.apply, undo, s.kind)])
          }
        | (_, Result.Step(_) as r, _, _) => r
        };

    let (and.):
      (requirements('a, 'c => 'b), requirement('c)) =>
      requirements(('a, 'c), 'b) =
      ((u, r1, env, v1), (r2, v2)) => (u(v2), r1 && r2, env, (v1, v2));

    let otherwise = (env, o) => (o, Result.BoxedValue, env, ());
    let update_test = (state, id, v) =>
      state := EvaluatorState.add_test(state^, id, v);
  };

  let rec evaluate_until = (flt_env, state, env, exp) => {
    Evaluator.EvaluatorEVMode.(
      {
        let u =
          switch (FilterMatcher.matches(~env, ~exp, ~act=Eval, flt_env)) {
          | Step => Indet(exp)
          | Eval =>
            switch (exp) {
            | Filter(flt, d1) =>
              let. _ = otherwise(env, (d1) => (Filter(flt, d1): DHExp.t))
              and. d1 =
                req_final(
                  evaluate_until(
                    FilterEnvironment.extends(flt, flt_env),
                    state,
                    env,
                  ),
                  d1 => Filter(flt, d1),
                  d1,
                );
              Step({apply: () => d1, kind: CompleteFilter, value: true});
            | _ =>
              Evaluator.Eval.transition(
                evaluate_until(flt_env),
                state,
                env,
                exp,
              )
            }
          };
        switch (u) {
        | BoxedValue(x) => BoxedValue(x)
        | Indet(x) => Indet(x)
        | Uneval(x) => evaluate_until(flt_env, state, env, x)
        };
      }
    );
  };

  module Decomp = Transition(DecomposeEVMode);
  let rec decompose = (flt_env, state, env, exp) => {
    switch (FilterMatcher.matches(~env, ~exp, ~act=Step, flt_env)) {
    | Step =>
      switch (exp) {
      | Filter(flt, d1) =>
        DecomposeEVMode.(
          {
            let. _ = otherwise(env, (d1) => (Filter(flt, d1): DHExp.t))
            and. d1 =
              req_final(
                decompose(
                  FilterEnvironment.extends(flt, flt_env),
                  state,
                  env,
                ),
                d1 => Filter(flt, d1),
                d1,
              );
            Step({apply: () => d1, kind: CompleteFilter, value: true});
          }
        )
      | _ => Decomp.transition(decompose(flt_env), state, env, exp)
      }
    | Eval =>
      Step([
        EvalObj.mk(
          Mark,
          () =>
            Evaluator.EvaluatorEVMode.unbox(
              evaluate_until(flt_env, state, env, exp),
            ),
          exp,
          Skip,
        ),
      ])
    };
  };
};

let rec rev_concat: (list('a), list('a)) => list('a) =
  (ls, rs) => {
    switch (ls) {
    | [] => rs
    | [hd, ...tl] => rev_concat(tl, [hd, ...rs])
    };
  };

let rec compose = (ctx: EvalCtx.t, d: DHExp.t): DHExp.t => {
  DHExp.(
    switch (ctx) {
    | Mark => d
    | Closure(env, ctx) =>
      let d = compose(ctx, d);
      Closure(env, d);
    | Filter(flt_env, ctx) =>
      let d = compose(ctx, d);
      Filter(flt_env, d);
    | Sequence1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Sequence(d1, d2);
    | Sequence2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Sequence(d1, d2);
    | Ap1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Ap(d1, d2);
    | Ap2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Ap(d1, d2);
    | ApBuiltin(s, ctx) =>
      let d' = compose(ctx, d);
      ApBuiltin(s, d');
    | Test(lit, ctx) =>
      let d1 = compose(ctx, d);
      Test(lit, d1);
    | BinBoolOp1(op, ctx, d2) =>
      let d1 = compose(ctx, d);
      BinBoolOp(op, d1, d2);
    | BinBoolOp2(op, d1, ctx) =>
      let d2 = compose(ctx, d);
      BinBoolOp(op, d1, d2);
    | BinIntOp1(op, ctx, d2) =>
      let d1 = compose(ctx, d);
      BinIntOp(op, d1, d2);
    | BinIntOp2(op, d1, ctx) =>
      let d2 = compose(ctx, d);
      BinIntOp(op, d1, d2);
    | BinFloatOp1(op, ctx, d2) =>
      let d1 = compose(ctx, d);
      BinFloatOp(op, d1, d2);
    | BinFloatOp2(op, d1, ctx) =>
      let d2 = compose(ctx, d);
      BinFloatOp(op, d1, d2);
    | BinStringOp1(op, ctx, d2) =>
      let d1 = compose(ctx, d);
      BinStringOp(op, d1, d2);
    | BinStringOp2(op, d1, ctx) =>
      let d2 = compose(ctx, d);
      BinStringOp(op, d1, d2);
    | Cons1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Cons(d1, d2);
    | Cons2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Cons(d1, d2);
    | ListConcat1(ctx, d2) =>
      let d1 = compose(ctx, d);
      ListConcat(d1, d2);
    | ListConcat2(d1, ctx) =>
      let d2 = compose(ctx, d);
      ListConcat(d1, d2);
    | Tuple(ctx, (ld, rd)) =>
      let d = compose(ctx, d);
      Tuple(rev_concat(ld, [d, ...rd]));
    | ListLit(m, i, t, ctx, (ld, rd)) =>
      let d = compose(ctx, d);
      ListLit(m, i, t, rev_concat(ld, [d, ...rd]));
    | Let1(dp, ctx, d2) =>
      let d = compose(ctx, d);
      Let(dp, d, d2);
    | Let2(dp, d1, ctx) =>
      let d = compose(ctx, d);
      Let(dp, d1, d);
    | Fun(dp, t, ctx, v) =>
      let d = compose(ctx, d);
      Fun(dp, t, d, v);
    | FixF(v, t, ctx) =>
      let d = compose(ctx, d);
      FixF(v, t, d);
    | Prj(ctx, n) =>
      let d = compose(ctx, d);
      Prj(d, n);
    | Cast(ctx, ty1, ty2) =>
      let d = compose(ctx, d);
      Cast(d, ty1, ty2);
    | FailedCast(ctx, ty1, ty2) =>
      let d = compose(ctx, d);
      FailedCast(d, ty1, ty2);
    | InvalidOperation(ctx, err) =>
      let d = compose(ctx, d);
      InvalidOperation(d, err);
    | NonEmptyHole(reason, u, i, ctx) =>
      let d = compose(ctx, d);
      NonEmptyHole(reason, u, i, d);
    | ConsistentCase(Case(ctx, rule, n)) =>
      let d = compose(ctx, d);
      ConsistentCase(Case(d, rule, n));
    | ConsistentCaseRule(scr, p, ctx, (lr, rr), n) =>
      let d = compose(ctx, d);
      ConsistentCase(
        Case(scr, rev_concat(lr, [(Rule(p, d): DHExp.rule), ...rr]), n),
      );
    | InconsistentBranches(u, i, Case(ctx, rule, n)) =>
      let d = compose(ctx, d);
      InconsistentBranches(u, i, Case(d, rule, n));
    | InconsistentBranchesRule(scr, mv, hi, p, ctx, (lr, rr), n) =>
      let d = compose(ctx, d);
      InconsistentBranches(
        mv,
        hi,
        Case(scr, rev_concat(lr, [(Rule(p, d): DHExp.rule), ...rr]), n),
      );
    }
  );
};

let decompose = (d: DHExp.t) => {
  let es = EvaluatorState.init;
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  let rs = Decompose.decompose([], ref(es), env, d);
  Decompose.Result.unbox(rs);
};

let rec evaluate_with_history = d =>
  switch (decompose(d)) {
  | [] => []
  | [x, ..._] =>
    let next = compose(x.ctx, x.apply());
    [next, ...evaluate_with_history(next)];
  };

module Stepper = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type step = {
    d: DHExp.t,
    step: EvalObj.t,
  };

  type step_with_previous = {
    step,
    previous: option(step),
    hidden: list(step),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: DHExp.t,
    previous: list(step),
    next: list(EvalObj.t),
  };

  let rec step_forward = (~settings, e: EvalObj.t, s: t) => {
    let current = compose(e.ctx, e.apply());
    skip_steps(
      ~settings,
      {
        current,
        previous: [{d: s.current, step: e}, ...s.previous],
        next: decompose(current),
      },
    );
  }
  and skip_steps = (~settings, s) => {
    switch (
      List.find_opt(
        (x: EvalObj.t) => should_hide_step(~settings, x.knd),
        s.next,
      )
    ) {
    | None => s
    | Some(e) => step_forward(~settings, e, s)
    };
  };

  let mk = (~settings, d) => {
    skip_steps(~settings, {current: d, previous: [], next: decompose(d)});
  };

  let rec undo_point =
          (~settings): (list(step) => option((step, list(step)))) =>
    fun
    | [] => None
    | [x, ...xs] when should_hide_step(~settings, x.step.knd) =>
      undo_point(~settings, xs)
    | [x, ...xs] => Some((x, xs));

  let step_backward = (~settings, s: t) =>
    switch (undo_point(~settings, s.previous)) {
    | None => failwith("cannot step backwards")
    | Some((x, xs)) => {current: x.d, previous: xs, next: decompose(x.d)}
    };

  let update_expr = (d: DHExp.t, _: t) => {
    current: d,
    previous: [],
    next: decompose(d),
  };

  let get_justification: step_kind => string =
    fun
    | LetBind => "substitution"
    | Sequence => "sequence"
    | FixUnwrap => "unroll fixpoint"
    | UpdateTest => "update test"
    | FunAp => "apply function"
    | BuiltinWrap => "wrap builtin"
    | BuiltinAp(s) => "evaluate " ++ s
    | BinIntOp(Plus | Minus | Times | Power | Divide)
    | BinFloatOp(Plus | Minus | Times | Power | Divide) => "arithmetic"
    | BinIntOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
    | BinFloatOp(
        LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual,
      ) => "comparison"
    | BinIntOp(Equals | NotEquals)
    | BinFloatOp(Equals | NotEquals)
    | BinStringOp(Equals) => "check equality"
    | BinStringOp(Concat) => "string manipulation"
    | BinBoolOp(_) => "boolean logic"
    | ListCons => "list manipulation"
    | ListConcat => "list manipulation"
    | CaseApply => "case selection"
    | CaseNext => "case discarding"
    | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
    | InvalidStep => "error"
    | VarLookup => "variable lookup"
    | CastAp
    | Cast => "cast calculus"
    | CompleteFilter => "unidentified step"
    | CompleteClosure => "unidentified step"
    | FunClosure => "unidentified step"
    | Skip => "skipped steps";

  let get_history = (~settings, stepper) => {
    let rec get_history':
      list(step) => (list(step), list(step_with_previous)) =
      fun
      | [] => ([], [])
      | [step, ...steps] => {
          let (hidden, ss) = get_history'(steps);
          if (should_hide_step(~settings, step.step.knd)) {
            ([step, ...hidden], ss);
          } else {
            (
              [],
              [
                {
                  step,
                  previous:
                    Option.map(
                      (x: step_with_previous) => x.step,
                      List.nth_opt(ss, 0),
                    ),
                  hidden,
                },
                ...ss,
              ],
            );
          };
        };
    stepper.previous |> get_history';
  };
};
