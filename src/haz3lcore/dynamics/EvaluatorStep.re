open Transition;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = {
  d: DHExp.t, // technically can be calculated from d_loc and ctx
  state: EvaluatorState.t,
  d_loc: DHExp.t, // the expression at the location given by ctx
  ctx: EvalCtx.t,
  knd: step_kind,
};

let unwrap = (step, sel: EvalCtx.cls) =>
  EvalCtx.unwrap(step.ctx, sel) |> Option.map(ctx => {...step, ctx});

let unwrap_unsafe = (step, sel: EvalCtx.cls) =>
  // TODO[Matt]: bring back "safe" version
  EvalCtx.unwrap(step.ctx, sel) |> Option.map(ctx => {...step, ctx});

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
    switch (exp) {
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
      Closure(env, d);
    | Filter(flt, ctx) =>
      let d = compose(ctx, d);
      Filter(flt, d);
    | Sequence1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Sequence(d1, d2);
    | Sequence2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Sequence(d1, d2);
    | TypAp(ctx, typ) =>
      let d1 = compose(ctx, d);
      TypAp(d1, typ);
    | Ap1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Ap(d1, d2);
    | Ap2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Ap(d1, d2);
    | ApBuiltin(s, ctx) =>
      let d' = compose(ctx, d);
      ApBuiltin(s, d');
    | IfThenElse1(c, ctx, d2, d3) =>
      let d' = compose(ctx, d);
      IfThenElse(c, d', d2, d3);
    | IfThenElse2(c, d1, ctx, d3) =>
      let d' = compose(ctx, d);
      IfThenElse(c, d1, d', d3);
    | IfThenElse3(c, d1, d2, ctx) =>
      let d' = compose(ctx, d);
      IfThenElse(c, d1, d2, d');
    | Test(lit, ctx) =>
      let d1 = compose(ctx, d);
      Test(lit, d1);
    | HintedTest(lit, ctx, hint1) =>
      let d1 = compose(ctx, d);
      HintedTest(lit, d1, hint1);
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
  let rs = Decompose.decompose(ref(es), env, d);
  Decompose.Result.unbox(rs);
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
  let composed = compose(ctx, exp);
  let (pact, pidx) = (act, idx);
  let (mact, midx) = FilterMatcher.matches(~env, ~exp=composed, ~act, flt);
  let (act, idx) =
    switch (ctx) {
    | Filter(_, _) => (pact, pidx)
    | _ => midx > pidx ? (mact, midx) : (pact, pidx)
    };
  let map = ((a, i, c), f: EvalCtx.t => EvalCtx.t) => {
    (a, i, f(c));
  };
  let (let+) = map;
  let (ract, ridx, rctx) =
    switch (ctx) {
    | Mark => (act, idx, EvalCtx.Mark)
    | Closure(env, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Closure(env, ctx);
    | Filter(Filter(flt'), ctx) =>
      let flt = flt |> FilterEnvironment.extends(flt');
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Filter(Filter(flt'), ctx);
    | Filter(Residue(idx', act'), ctx) =>
      let (ract, ridx, rctx) =
        if (idx > idx') {
          matches(env, flt, ctx, exp, act, idx);
        } else {
          matches(env, flt, ctx, exp, act', idx');
        };
      if (act' |> snd == All) {
        (ract, ridx, Filter(Residue(idx', act'), rctx));
      } else {
        (ract, ridx, rctx);
      };
    | Sequence1(ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Sequence1(ctx, d2);
    | Sequence2(d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Sequence2(d1, ctx);
    | Let1(d1, ctx, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Let1(d1, ctx, d3);
    | Let2(d1, d2, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Let2(d1, d2, ctx);
    | Fun(dp, ty, ctx, name) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Fun(dp, ty, ctx, name);
    | FixF(name, ty, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      FixF(name, ty, ctx);
    | Ap1(ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Ap1(ctx, d2);
    | Ap2(d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Ap2(d1, ctx);
    | IfThenElse1(c, ctx, d2, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      IfThenElse1(c, ctx, d2, d3);
    | IfThenElse2(c, d1, ctx, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      IfThenElse2(c, d1, ctx, d3);
    | IfThenElse3(c, d1, d2, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      IfThenElse3(c, d1, d2, ctx);
    | BinBoolOp1(op, ctx, d1) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinBoolOp1(op, ctx, d1);
    | BinBoolOp2(op, d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinBoolOp2(op, d1, ctx);
    | BinIntOp1(op, ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinIntOp1(op, ctx, d2);
    | BinIntOp2(op, d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinIntOp2(op, d1, ctx);
    | BinFloatOp1(op, ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinFloatOp1(op, ctx, d2);
    | BinFloatOp2(op, d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinFloatOp2(op, d1, ctx);
    | BinStringOp1(op, ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinStringOp1(op, ctx, d2);
    | BinStringOp2(op, d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinStringOp2(op, d1, ctx);
    | Tuple(ctx, ds) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Tuple(ctx, ds);
    | ApBuiltin(name, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      ApBuiltin(name, ctx);
    | Test(id, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Test(id, ctx);
    | HintedTest(id, ctx, hint) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      HintedTest(id, ctx, hint);
    | ListLit(u, i, ty, ctx, ds) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      ListLit(u, i, ty, ctx, ds);
    | Cons1(ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Cons1(ctx, d2);
    | Cons2(d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Cons2(d1, ctx);
    | ListConcat1(ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      ListConcat1(ctx, d2);
    | ListConcat2(d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      ListConcat2(d1, ctx);
    | Prj(ctx, n) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Prj(ctx, n);
    | NonEmptyHole(e, u, i, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      NonEmptyHole(e, u, i, ctx);
    | Cast(ctx, ty, ty') =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Cast(ctx, ty, ty');
    | FailedCast(ctx, ty, ty') =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      FailedCast(ctx, ty, ty');
    | InvalidOperation(ctx, error) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      InvalidOperation(ctx, error);
    | ConsistentCase(Case(ctx, rs, i)) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      ConsistentCase(Case(ctx, rs, i));
    | ConsistentCaseRule(dexp, dpat, ctx, rs, i) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      ConsistentCaseRule(dexp, dpat, ctx, rs, i);
    | InconsistentBranches(u, i, Case(ctx, rs, ri)) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      InconsistentBranches(u, i, Case(ctx, rs, ri));
    | InconsistentBranchesRule(dexp, u, i, dpat, ctx, rs, ri) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      InconsistentBranchesRule(dexp, u, i, dpat, ctx, rs, ri);
    | TypAp(ctx, ty) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      TypAp(ctx, ty);
    };
  switch (ctx) {
  | Filter(_) => (ract, ridx, rctx)
  | _ when midx > pidx && mact |> snd == All => (
      ract,
      ridx,
      Filter(Residue(midx, mact), rctx),
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

let decompose = (~settings, d) =>
  d |> decompose |> List.map(should_hide_eval_obj(~settings));

let evaluate_with_history = (~settings, d) => {
  let state = ref(EvaluatorState.init);
  let rec go = d =>
    switch (decompose(~settings, d)) {
    | [] => []
    | [(_, x), ..._] =>
      switch (take_step(state, x.env, x.d_loc)) {
      | None => []
      | Some(d) =>
        let next = compose(x.ctx, d);
        [next, ...go(next)];
      }
    };
  go(d);
};
