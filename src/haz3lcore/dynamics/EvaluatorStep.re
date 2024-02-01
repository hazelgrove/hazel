open Transition;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = {
  d: DHExp.t, // technically can be calculated from d_loc and ctx
  state: EvaluatorState.t,
  d_loc: DHExp.t, // the expression at the location given by ctx
  d_loc': DHExp.t,
  ctx: EvalCtx.t,
  knd: step_kind,
};

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t, // technically can be calculated from ctx
    d_loc: DHExp.t,
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
        | (undo, Result.BoxedValue, env, v) =>
          switch (rl(v)) {
          | Constructor => Result.BoxedValue
          | Indet => Result.Indet
          | Step(s) => Result.Step([EvalObj.mk(Mark, env, undo, s.kind)])
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
    let (term, rewrap) = DHExp.unwrap(exp);
    switch (term) {
    | DHExp.Filter(flt, d1) =>
      DecomposeEVMode.(
        {
          let. _ =
            otherwise(env, (d1) => (Filter(flt, d1) |> rewrap: DHExp.t))
          and. d1 =
            req_final(decompose(state, env), d1 => Filter(flt, d1), d1);
          Step({apply: () => d1, kind: CompleteFilter, value: true});
        }
      )
    | _ => Decomp.transition(decompose, state, env, exp)
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
    let req_value = (_, _, d) => d;
    let req_all_value = (_, _, ds) => ds;
    let req_final = (_, _, d) => d;
    let req_all_final = (_, _, ds) => ds;

    let (let.) = (rq: requirements('a, DHExp.t), rl: 'a => rule) =>
      switch (rl(rq)) {
      | Step({apply, _}) => Some(apply())
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
      Closure(env, d) |> fresh;
    | Filter(flt, ctx) =>
      let d = compose(ctx, d);
      Filter(flt, d) |> fresh;
    | Seq1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Seq(d1, d2) |> fresh;
    | Seq2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Seq(d1, d2) |> fresh;
    | Ap1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Ap(d1, d2) |> fresh;
    | Ap2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Ap(d1, d2) |> fresh;
    | ApBuiltin(s, ctx) =>
      let d' = compose(ctx, d);
      ApBuiltin(s, d') |> fresh;
    | If1(c, ctx, d2, d3) =>
      let d' = compose(ctx, d);
      If(c, d', d2, d3) |> fresh;
    | If2(c, d1, ctx, d3) =>
      let d' = compose(ctx, d);
      If(c, d1, d', d3) |> fresh;
    | If3(c, d1, d2, ctx) =>
      let d' = compose(ctx, d);
      If(c, d1, d2, d') |> fresh;
    | Test(lit, ctx) =>
      let d1 = compose(ctx, d);
      Test(lit, d1) |> fresh;
    | BinOp1(op, ctx, d2) =>
      let d1 = compose(ctx, d);
      BinOp(op, d1, d2) |> fresh;
    | BinOp2(op, d1, ctx) =>
      let d2 = compose(ctx, d);
      BinOp(op, d1, d2) |> fresh;
    | Cons1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Cons(d1, d2) |> fresh;
    | Cons2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Cons(d1, d2) |> fresh;
    | ListConcat1(ctx, d2) =>
      let d1 = compose(ctx, d);
      ListConcat(d1, d2) |> fresh;
    | ListConcat2(d1, ctx) =>
      let d2 = compose(ctx, d);
      ListConcat(d1, d2) |> fresh;
    | Tuple(ctx, (ld, rd)) =>
      let d = compose(ctx, d);
      Tuple(rev_concat(ld, [d, ...rd])) |> fresh;
    | ListLit(m, i, t, ctx, (ld, rd)) =>
      let d = compose(ctx, d);
      ListLit(m, i, t, rev_concat(ld, [d, ...rd])) |> fresh;
    | Let1(dp, ctx, d2) =>
      let d = compose(ctx, d);
      Let(dp, d, d2) |> fresh;
    | Let2(dp, d1, ctx) =>
      let d = compose(ctx, d);
      Let(dp, d1, d) |> fresh;
    | Fun(dp, t, ctx, env, v) =>
      let d = compose(ctx, d);
      Fun(dp, t, d, env, v) |> fresh;
    | FixF(v, t, ctx) =>
      let d = compose(ctx, d);
      FixF(v, t, d) |> fresh;
    | Prj(ctx, n) =>
      let d = compose(ctx, d);
      Prj(d, n) |> fresh;
    | Cast(ctx, ty1, ty2) =>
      let d = compose(ctx, d);
      Cast(d, ty1, ty2) |> fresh;
    | FailedCast(ctx, ty1, ty2) =>
      let d = compose(ctx, d);
      FailedCast(d, ty1, ty2) |> fresh;
    | InvalidOperation(ctx, err) =>
      let d = compose(ctx, d);
      InvalidOperation(d, err) |> fresh;
    | NonEmptyHole(reason, u, i, ctx) =>
      let d = compose(ctx, d);
      NonEmptyHole(reason, u, i, d) |> fresh;
    | MatchScrut(c, ctx, rules) =>
      let d = compose(ctx, d);
      Match(c, d, rules) |> fresh;
    | MatchRule(c, scr, p, ctx, (lr, rr)) =>
      let d = compose(ctx, d);
      Match(c, scr, rev_concat(lr, [(p, d), ...rr])) |> fresh;
    }
  );
};

let decompose = (d: DHExp.t) => {
  let es = EvaluatorState.init;
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  let rs = Decompose.decompose(ref(es), env, d);
  Decompose.Result.unbox(rs);
};

let evaluate_with_history = d => {
  let state = ref(EvaluatorState.init);
  let rec go = d =>
    switch (decompose(d)) {
    | [] => []
    | [x, ..._] =>
      switch (take_step(state, x.env, x.d_loc)) {
      | None => []
      | Some(d) =>
        let next = compose(x.ctx, d);
        [next, ...go(next)];
      }
    };
  go(d);
};
