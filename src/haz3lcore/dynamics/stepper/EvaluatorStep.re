open Util;
open Transition;

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t, // technically can be calculated from ctx
    d_loc: DHExp.t,
    ctx: EvalCtx.t,
    knd: step_kind,
  };

  let mk = (ctx, env, d_loc, knd) => {
    ctx,
    env,
    d_loc,
    knd,
  };

  let wrap = (f: EvalCtx.t => EvalCtx.t, obj: t) => {
    ...obj,
    ctx: obj.ctx |> f,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    old_id: Id.t, // The id of the term about to be stepped
    new_id: Id.t, // The id of the term after it is stepped
    knd: step_kind,
  };
};

let rec matches =
        (
          env: ClosureEnvironment.t,
          flt: FilterEnvironment.t,
          ctx: EvalCtx.t,
          exp: DHExp.t,
          act: FilterAction.t,
          idx: int,
        )
        : (FilterAction.t, int, EvalCtx.t) => {
  let composed = EvalCtx.compose(ctx, exp);
  let (pact, pidx) = (act, idx);
  let (mact, midx) = FilterMatcher.matches(~env, ~exp=composed, ~act, flt);
  let (act, idx) =
    switch (ctx) {
    | Term({term: Filter(_, _), _}) => (pact, pidx)
    | _ => midx > pidx ? (mact, midx) : (pact, pidx)
    };
  let map = ((a, i, c), f) => {
    (a, i, f(c));
  };
  let (let+) = map;
  let (ract, ridx, rctx) =
    switch (ctx) {
    | Mark => (act, idx, EvalCtx.Mark)
    | Term({term, ids}) =>
      let rewrap = term =>
        EvalCtx.Term({
          term,
          ids,
        });
      switch ((term: EvalCtx.term)) {
      | Closure(env, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Closure(env, ctx) |> rewrap;
      | Filter(Filter(flt'), ctx) =>
        let flt = flt |> FilterEnvironment.extends(flt');
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Filter(Filter(flt'), ctx) |> rewrap;
      | Filter(Residue(idx, act), ctx) =>
        let (ract, ridx, rctx) = matches(env, flt, ctx, exp, act, idx);
        if (ridx == idx && ract |> snd == All) {
          (ract, ridx, Filter(Residue(idx, act), rctx) |> rewrap);
        } else {
          (ract, ridx, rctx);
        };
      | Seq1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Seq1(ctx, d2) |> rewrap;
      | Seq2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Seq2(d1, ctx) |> rewrap;
      | Let1(d1, ctx, d3) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Let1(d1, ctx, d3) |> rewrap;
      | Let2(d1, d2, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Let2(d1, d2, ctx) |> rewrap;
      | Fun(dp, ctx, ty, name) =>
        // TODO: Should this env include the bound variables?
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Fun(dp, ctx, ty, name) |> rewrap;
      | FixF(name, ctx, env') =>
        let+ ctx =
          matches(Option.value(~default=env, env'), flt, ctx, exp, act, idx);
        FixF(name, ctx, env') |> rewrap;
      | Ap1(dir, ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Ap1(dir, ctx, d2) |> rewrap;
      | Ap2(dir, d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Ap2(dir, d1, ctx) |> rewrap;
      | TypAp(ctx, ty) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        TypAp(ctx, ty) |> rewrap;
      | DeferredAp1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        DeferredAp1(ctx, d2) |> rewrap;
      | DeferredAp2(d1, ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        DeferredAp2(d1, ctx, ds) |> rewrap;
      | If1(ctx, d2, d3) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        If1(ctx, d2, d3) |> rewrap;
      | If2(d1, ctx, d3) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        If2(d1, ctx, d3) |> rewrap;
      | If3(d1, d2, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        If3(d1, d2, ctx) |> rewrap;
      | UnOp(op, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        UnOp(op, ctx) |> rewrap;
      | BinOp1(op, ctx, d1) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        BinOp1(op, ctx, d1) |> rewrap;
      | BinOp2(op, d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        BinOp2(op, d1, ctx) |> rewrap;
      | Tuple(ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Tuple(ctx, ds) |> rewrap;
      | TupLabel(label, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        TupLabel(label, ctx) |> rewrap;
      | Dot1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Dot1(ctx, d2) |> rewrap;
      | Dot2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Dot2(d1, ctx) |> rewrap;
      | Test(ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Test(ctx) |> rewrap;
      | HintedTest(ctx, hint) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        HintedTest(ctx, hint) |> rewrap;
      | Parens(ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Parens(ctx) |> rewrap;
      | Probe(ctx, pr) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Probe(ctx, pr) |> rewrap;
      | ListLit(ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        ListLit(ctx, ds) |> rewrap;
      | Cons1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Cons1(ctx, d2) |> rewrap;
      | Cons2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Cons2(d1, ctx) |> rewrap;
      | ListConcat1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        ListConcat1(ctx, d2) |> rewrap;
      | ListConcat2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        ListConcat2(d1, ctx) |> rewrap;
      | MultiHole(ctx, (dl, dr)) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        MultiHole(ctx, (dl, dr)) |> rewrap;
      | Cast(ctx, ty, ty') =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Cast(ctx, ty, ty') |> rewrap;
      | FailedCast(ctx, ty, ty') =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        FailedCast(ctx, ty, ty') |> rewrap;
      | DynamicErrorHole(ctx, error) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        DynamicErrorHole(ctx, error) |> rewrap;
      | MatchScrut(ctx, rs) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        MatchScrut(ctx, rs) |> rewrap;
      | MatchRule(scr, p, ctx, rs) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        MatchRule(scr, p, ctx, rs) |> rewrap;
      };
    };
  switch (ctx) {
  | Term({term: Filter(_), _}) => (ract, ridx, rctx)
  | _ when midx == ridx && midx > pidx && mact |> snd == All => (
      ract,
      ridx,
      Term({
        term: Filter(Residue(midx, mact), rctx),
        ids: [Id.mk()],
      }),
    )
  | _ => (ract, ridx, rctx)
  };
};

let should_hide_eval_obj =
    (~settings, x: EvalObj.t): (FilterAction.action, EvalObj.t) =>
  if (should_hide_step_kind(~settings, x.knd)) {
    (Eval, x);
  } else {
    let (act, _, ctx) =
      matches(ClosureEnvironment.empty, [], x.ctx, x.d_loc, (Step, One), 0);
    switch (act) {
    | (Eval, _) => (
        Eval,
        {
          ...x,
          ctx,
        },
      )
    | (Step, _) => (
        Step,
        {
          ...x,
          ctx,
        },
      )
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

  module DecomposeEVMode: {
    include
      EV_MODE with
        type result = Result.t and type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t);
    type requirement('a) = (Result.t, 'a);
    type requirements('a, 'b) = ('b, Result.t, ClosureEnvironment.t, 'a);
    type result = Result.t;

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
        | (undo, Result.BoxedValue, env, v) =>
          switch (rl(v)) {
          | Constructor => Result.BoxedValue
          | Value => Result.BoxedValue
          | Indet => Result.Indet
          | Step(s) => Result.Step([EvalObj.mk(Mark, env, undo, s.kind)])
          // TODO: Actually show these exceptions to the user!
          | exception (EvaluatorError.Exception(_)) => Result.Indet
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
    let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
      state := EvaluatorState.add_closure(state^, closure);
  };

  module Decomp = Transition(DecomposeEVMode);
  let rec decompose = (~in_closure=?, state, env, exp) => {
    switch (exp) {
    | _ =>
      Decomp.transition(
        decompose,
        ~in_closure=Option.value(~default=() => (), in_closure),
        ~mode=`Substitution,
        state,
        env,
        exp,
      )
    };
  };
};

module TakeStep = {
  module TakeStepEVMode: {
    include
      EV_MODE with
        type result = option(DHExp.t) and type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t);
    type requirement('a) = 'a;
    type requirements('a, 'b) = 'a;
    type result = option(DHExp.t);

    // Assume that everything is either value or final as required.
    let req_final = (_, _, d) => d;
    let req_all_final = (_, _, ds) => ds;

    let (let.) = (rq: requirements('a, DHExp.t), rl: 'a => rule) =>
      switch (rl(rq)) {
      | Step({expr, state_update, _}) =>
        state_update();
        Some(expr);
      | Constructor
      | Value
      | Indet => None
      };

    let (and.) = (x1, x2) => (x1, x2);

    let otherwise = (_, _) => ();

    let update_test = (state, id, v) =>
      state := EvaluatorState.add_test(state^, id, v);

    let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
      state := EvaluatorState.add_closure(state^, closure);
  };

  module TakeStepEV = Transition(TakeStepEVMode);

  let take_step = (~in_closure=?, state, env, d) =>
    TakeStepEV.transition(
      (~in_closure as _=?, _, _, _) => None,
      ~in_closure=Option.value(~default=() => (), in_closure),
      ~mode=`Substitution,
      state,
      env,
      d,
    )
    |> Option.map(DHExp.replace_all_ids);
};

let take_step = TakeStep.take_step;

let decompose = (d: DHExp.t, es: EvaluatorState.t) => {
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  let rs = Decompose.decompose(ref(es), env, d);
  Decompose.Result.unbox(rs);
};

/* ========== PUBLIC METHODS ========== */
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = EvalObj.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | AutoStep(step)
  | AvailableSteps(list(step));

let get_status = (~settings: CoreSettings.t, exp, state) => {
  print_endline("EXP: " ++ (exp |> Exp.show));
  let eos =
    decompose(exp, state)
    |> List.map(should_hide_eval_obj(~settings=settings.evaluation)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
  switch (List.find_opt(((x, _)) => x == FilterAction.Eval, eos)) {
  | Some((_, x)) => AutoStep(x)
  | None => AvailableSteps(List.map(((_, x)) => x, eos))
  };
};

let get_step_id = (step: step): Id.t => step.d_loc |> DHExp.rep_id;

let get_step_kind = (step: step): step_kind => step.knd;

let take_step = (step: EvalObj.t) => {
  let state = ref(EvaluatorState.init); // HACK: state isn't actually carried through the stepper...
  let+ next_expr = take_step(state, step.env, step.d_loc);
  let next_expr = {
    ...next_expr,
    annotation: IdTagged.IdTag.{ids: step.d_loc |> IdTagged.ids},
  };
  let next_state = state^;
  let next_expr =
    EvalCtx.compose(step.ctx, next_expr)
    |> Exp.replace_all_ids
    |> DHExp.substitute_closures(Builtins.env_init);
  (next_expr, next_state);
};

let refresh_step =
    (
      ~settings: CoreSettings.t,
      exp: Exp.t,
      state: EvaluatorState.t,
      step: step,
    ) => {
  let eos =
    decompose(exp, state)
    |> List.map(should_hide_eval_obj(~settings=settings.evaluation)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
  let* (_, x) =
    List.find_opt(
      ((_, step': step)) =>
        IdTagged.ids(step'.d_loc) == IdTagged.ids(step.d_loc),
      eos,
    );
  Some(x);
};
