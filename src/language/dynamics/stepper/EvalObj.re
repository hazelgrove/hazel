open Transition;

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
      | Asc(ctx, ty) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Asc(ctx, ty) |> rewrap;
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

let should_hide_eval_obj = (~settings, x: t): (FilterAction.action, t) =>
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
