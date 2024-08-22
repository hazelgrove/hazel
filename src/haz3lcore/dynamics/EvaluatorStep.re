open Transition;
open Sexplib.Conv;
// open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = {
  d: DHExp.t(IdTag.t), // technically can be calculated from d_loc and ctx
  state: EvaluatorState.t,
  d_loc: DHExp.t(IdTag.t), // the expression at the location given by ctx
  d_loc': DHExp.t(IdTag.t),
  ctx: EvalCtx.t,
  knd: step_kind,
};

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t, // technically can be calculated from ctx
    d_loc: DHExp.t(IdTag.t),
    ctx: EvalCtx.t,
    knd: step_kind,
  };

  let mk = (ctx, env, d_loc, knd) => {ctx, env, d_loc, knd};

  let get_ctx = (obj: t): EvalCtx.t => {
    obj.ctx;
  };
  let get_kind = (obj: t): step_kind => obj.knd;

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

  module DecomposeEVMode: {
    include
      EV_MODE with
        type result = Result.t and type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t);
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

    let req_final_or_value = (cont, wr, d) => {
      switch (cont(d)) {
      | Result.Indet => (Result.BoxedValue, (d, false))
      | Result.BoxedValue => (Result.BoxedValue, (d, true))
      | Result.Step(objs) => (
          Result.Step(List.map(EvalObj.wrap(wr), objs)),
          (d, false),
        )
      };
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

    let (let.): (requirements('a, DHExp.t(IdTag.t)), 'a => rule) => result =
      (rq, rl) =>
        switch (rq) {
        | (_, Result.Indet, _, _) => Result.Indet
        | (undo, Result.BoxedValue, env, v) =>
          switch (rl(v)) {
          | Constructor => Result.BoxedValue
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
  };

  module Decomp = Transition(DecomposeEVMode);
  let rec decompose = (state, env, exp) => {
    switch (exp) {
    | _ => Decomp.transition(decompose, state, env, exp)
    };
  };
};

module TakeStep = {
  module TakeStepEVMode: {
    include
      EV_MODE with
        type result = option(DHExp.t(IdTag.t)) and
        type state = ref(EvaluatorState.t);
  } = {
    type state = ref(EvaluatorState.t);
    type requirement('a) = 'a;
    type requirements('a, 'b) = 'a;
    type result = option(DHExp.t(IdTag.t));

    // Assume that everything is either value or final as required.
    let req_value = (_, _, d) => d;
    let req_all_value = (_, _, ds) => ds;
    let req_final = (_, _, d) => d;
    let req_all_final = (_, _, ds) => ds;

    let req_final_or_value = (_, _, d) => (d, true);

    let (let.) = (rq: requirements('a, DHExp.t(IdTag.t)), rl: 'a => rule) =>
      switch (rl(rq)) {
      | Step({expr, state_update, _}) =>
        state_update();
        Some(expr);
      | Constructor
      | Indet => None
      };

    let (and.) = (x1, x2) => (x1, x2);

    let otherwise = (_, _) => ();

    let update_test = (state, id, v) =>
      state := EvaluatorState.add_test(state^, id, v);
  };

  module TakeStepEV = Transition(TakeStepEVMode);

  let take_step = (state, env, d) =>
    TakeStepEV.transition((_, _, _) => None, state, env, d);
};

let take_step = TakeStep.take_step;

let decompose = (d: DHExp.t(IdTag.t), es: EvaluatorState.t) => {
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  let rs = Decompose.decompose(ref(es), env, d);
  Decompose.Result.unbox(rs);
};

let rec matches =
        (
          env: ClosureEnvironment.t,
          flt: FilterEnvironment.t(IdTag.t),
          ctx: EvalCtx.t,
          exp: DHExp.t(IdTag.t),
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
  let map = ((a, i, c), f: EvalCtx.t => EvalCtx.t) => {
    (a, i, f(c));
  };
  let (let+) = map;
  let (ract, ridx, rctx) = {
    let wrap_ids = (ids, ctx) => EvalCtx.Term({term: ctx, ids});
    switch (ctx) {
    | Mark => (act, idx, EvalCtx.Mark)
    | Term({term, ids}) =>
      switch (term) {
      | Closure(env, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Closure(env, ctx) |> wrap_ids(ids);
      | Filter(Filter(flt'), ctx) =>
        let flt = flt |> FilterEnvironment.extends(flt');
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Filter(Filter(flt'), ctx) |> wrap_ids(ids);
      | Filter(Residue(idx', act'), ctx) =>
        let (ract, ridx, rctx) =
          if (idx > idx') {
            matches(env, flt, ctx, exp, act, idx);
          } else {
            matches(env, flt, ctx, exp, act', idx');
          };
        if (act' |> snd == All) {
          (
            ract,
            ridx,
            Term({
              term: Filter(Residue(idx', act'), rctx),
              ids: [Id.mk()],
            }),
          );
        } else {
          (ract, ridx, rctx);
        };
      | Seq1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Seq1(ctx, d2) |> wrap_ids(ids);
      | Seq2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Seq2(d1, ctx) |> wrap_ids(ids);
      | Let1(d1, ctx, d3) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Let1(d1, ctx, d3) |> wrap_ids(ids);
      | Let2(d1, d2, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Let2(d1, d2, ctx) |> wrap_ids(ids);
      | Fun(dp, ctx, env', name) =>
        let+ ctx =
          matches(
            env' |> Option.value(~default=env),
            flt,
            ctx,
            exp,
            act,
            idx,
          );
        Fun(dp, ctx, env', name) |> wrap_ids(ids);
      | FixF(name, ctx, env') =>
        let+ ctx =
          matches(
            env' |> Option.value(~default=env),
            flt,
            ctx,
            exp,
            act,
            idx,
          );
        FixF(name, ctx, env') |> wrap_ids(ids);
      | Ap1(dir, ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Ap1(dir, ctx, d2) |> wrap_ids(ids);
      | Ap2(dir, d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Ap2(dir, d1, ctx) |> wrap_ids(ids);
      | If1(ctx, d2, d3) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        If1(ctx, d2, d3) |> wrap_ids(ids);
      | If2(d1, ctx, d3) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        If2(d1, ctx, d3) |> wrap_ids(ids);
      | If3(d1, d2, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        If3(d1, d2, ctx) |> wrap_ids(ids);
      | BinOp1(op, ctx, d1) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        BinOp1(op, ctx, d1) |> wrap_ids(ids);
      | BinOp2(op, d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        BinOp2(op, d1, ctx) |> wrap_ids(ids);
      | Test(ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Test(ctx) |> wrap_ids(ids);
      | ListLit(ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        ListLit(ctx, ds) |> wrap_ids(ids);
      | Tuple(ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Tuple(ctx, ds) |> wrap_ids(ids);
      | MultiHole(ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        MultiHole(ctx, ds) |> wrap_ids(ids);
      | Cons1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Cons1(ctx, d2) |> wrap_ids(ids);
      | Cons2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Cons2(d1, ctx) |> wrap_ids(ids);
      | ListConcat1(ctx, d2) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        ListConcat1(ctx, d2) |> wrap_ids(ids);
      | ListConcat2(d1, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        ListConcat2(d1, ctx) |> wrap_ids(ids);
      | Cast(ctx, ty, ty') =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Cast(ctx, ty, ty') |> wrap_ids(ids);
      | FailedCast(ctx, ty, ty') =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        FailedCast(ctx, ty, ty') |> wrap_ids(ids);
      | DynamicErrorHole(ctx, error) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        DynamicErrorHole(ctx, error) |> wrap_ids(ids);
      | MatchScrut(ctx, rs) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        MatchScrut(ctx, rs) |> wrap_ids(ids);
      | MatchRule(dexp, dpat, ctx, rs) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        MatchRule(dexp, dpat, ctx, rs) |> wrap_ids(ids);
      | TypAp(ctx, ty) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        TypAp(ctx, ty) |> wrap_ids(ids);
      | DeferredAp1(ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        DeferredAp1(ctx, ds) |> wrap_ids(ids);
      | DeferredAp2(d1, ctx, ds) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        DeferredAp2(d1, ctx, ds) |> wrap_ids(ids);
      | UnOp(op, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        UnOp(op, ctx) |> wrap_ids(ids);
      }
    };
  };
  switch (ctx) {
  | Term({term: Filter(_), _}) => (ract, ridx, rctx)
  | _ when midx > pidx && mact |> snd == All => (
      ract,
      ridx,
      Term({term: Filter(Residue(midx, mact), rctx), ids: [Id.mk()]}),
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
    | (Eval, _) => (Eval, {...x, ctx})
    | (Step, _) => (Step, {...x, ctx})
    };
  };

let should_hide_step = (~settings, x: step): (FilterAction.action, step) =>
  if (should_hide_step_kind(~settings, x.knd)) {
    (Eval, x);
  } else {
    let (act, _, ctx) =
      matches(ClosureEnvironment.empty, [], x.ctx, x.d_loc, (Step, One), 0);
    switch (act) {
    | (Eval, _) => (Eval, {...x, ctx})
    | (Step, _) => (Step, {...x, ctx})
    };
  };

let decompose = (~settings, d, st) =>
  decompose(d, st) |> List.map(should_hide_eval_obj(~settings));

let evaluate_with_history = (~settings, d) => {
  let state = ref(EvaluatorState.init);
  let rec go = d =>
    switch (decompose(~settings, d, state^)) {
    | [] => []
    | [(_, x), ..._] =>
      switch (take_step(state, x.env, x.d_loc)) {
      | None => []
      | Some(d) =>
        let next = EvalCtx.compose(x.ctx, d);
        [next, ...go(next)];
      }
    };
  go(d);
};
