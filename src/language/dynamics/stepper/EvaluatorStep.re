open Util;
open Transition;

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: Environment.t(Exp.t), // technically can be calculated from ctx
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
    exp_idx: int, // The index of the expression being stepped
    at_exp: Exp.t // The expression being stepped
  };

  let persist = (obj: t): persistent => {
    let full_exp = EvalCtx.compose(obj.ctx, obj.d_loc);
    {
      exp_idx: ProofHacks.exp_idx(obj.d_loc, full_exp),
      at_exp: obj.d_loc,
    };
  };
};

/* The context is rebuilt by the path walk below. Keep the action's
   priority separate from the level already represented by a residue. */
module State = {
  type t = {
    act: FilterAction.t,
    act_lvl: int,
    ins_lvl: int,
  };
};

/* Filter patterns retain the environment of their definition when the
   candidate path later crosses a closure with a different environment. */
module ScopedFilters = {
  type t = list((TermBase.filter, Environment.t(Exp.t)));
};

/* Equal levels keep the action already selected. */
let bump = ((act, level), (candidate, candidate_level)) =>
  candidate_level > level ? (candidate, candidate_level) : (act, level);

/* Filters are innermost first. A one-shot match may decide the action,
   but scanning continues to find the persistent match whose own action
   must be recorded. At most one residue is added at this position. */
let lookup =
    (env, flt: ScopedFilters.t, exp, s: State.t)
    : (State.t, option((int, FilterAction.t))) => {
  let rec scan = (flt: ScopedFilters.t, level, s: State.t) =>
    switch (flt) {
    | [] => (s, None)
    | _ when level <= s.ins_lvl => (s, None)
    | [(f, definition_env), ...rest] =>
      if (FilterMatcher.matches_exp(
            ~denv=env,
            exp,
            ~fenv=definition_env,
            f.pat,
          )) {
        let (act, act_lvl) = bump((s.act, s.act_lvl), (f.act, level));
        let s = {
          ...s,
          act,
          act_lvl,
        };
        switch (f.act |> snd) {
        | One => scan(rest, level - 1, s)
        | All => (
            {
              ...s,
              ins_lvl: level,
            },
            Some((level, f.act)),
          )
        };
      } else {
        scan(rest, level - 1, s);
      }
    };
  scan(flt, List.length(flt), s);
};

/* Coverage, not the winning action, determines whether an existing
   residue remains around its body. Preserve its original syntax ids. */
let keep = (ins_lvl, (level, act), ids, ctx): EvalCtx.t =>
  level <= ins_lvl
    ? ctx
    : Term({
        term: Filter(Residue(level, act), ctx),
        ids,
      });

let matches =
    (
      ~complete_filter=false,
      env: Environment.t(Exp.t),
      flt: FilterEnvironment.t,
      ctx: EvalCtx.t,
      exp: DHExp.t,
      act: FilterAction.t,
      idx: int,
    )
    : (FilterAction.t, int, EvalCtx.t) => {
  let rec walk =
          (env, flt, ctx: EvalCtx.t, exp: DHExp.t, s: State.t)
          : (State.t, EvalCtx.t) => {
    let (s, residue) =
      switch (ctx) {
      | Term({term: Filter(_), _}) => (s, None)
      | Mark when complete_filter => (s, None)
      | _ => lookup(env, flt, EvalCtx.compose(ctx, exp), s)
      };
    let map = ((s, ctx), f) => (s, f(ctx));
    let (let+) = map;
    let (result, rctx) =
      switch (ctx) {
      | Mark when complete_filter =>
        /* Only the cleanup leaf skips lookup. Ancestors have already
           propagated their actions and residues along this path. */
        let act_lvl =
          switch (exp.term) {
          | Filter(Residue(level, act), _) =>
            bump((s.act, s.act_lvl), (act, level)) |> snd
          | _ => s.act_lvl
          };
        (
          {
            ...s,
            act: (Eval, One),
            act_lvl,
          },
          EvalCtx.Mark,
        );
      | Mark => (s, EvalCtx.Mark)
      | Term({term, ids}) =>
        let rewrap = term =>
          EvalCtx.Term({
            term,
            ids,
          });
        switch ((term: EvalCtx.term)) {
        | Closure(env, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Closure(env, ctx) |> rewrap;
        | Filter(Unresolved(fexp), ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Filter(Unresolved(fexp), ctx) |> rewrap;
        | Filter(Filter(flt'), ctx) =>
          let flt = [(flt', env), ...flt];
          let+ ctx = walk(env, flt, ctx, exp, s);
          Filter(Filter(flt'), ctx) |> rewrap;
        | Filter(Residue(level, act), ctx) =>
          let (next_act, next_level) =
            bump((s.act, s.act_lvl), (act, level));
          let next: State.t = {
            act: next_act,
            act_lvl: next_level,
            ins_lvl: max(s.ins_lvl, level),
          };
          let (result, ctx) = walk(env, flt, ctx, exp, next);
          (result, keep(s.ins_lvl, (level, act), ids, ctx));
        | Seq1(ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Seq1(ctx, d2) |> rewrap;
        | Seq2(d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Seq2(d1, ctx) |> rewrap;
        | Let1(d1, ctx, d3) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Let1(d1, ctx, d3) |> rewrap;
        | Let2(d1, d2, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Let2(d1, d2, ctx) |> rewrap;
        | Theorem(dp, d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Theorem(dp, d1, ctx) |> rewrap;
        | Fun(dp, ctx, ty, name) =>
          // TODO: Should this env include the bound variables?
          let+ ctx = walk(env, flt, ctx, exp, s);
          Fun(dp, ctx, ty, name) |> rewrap;
        | FixF(name, ctx, env') =>
          let+ ctx =
            walk(Option.value(~default=env, env'), flt, ctx, exp, s);
          FixF(name, ctx, env') |> rewrap;
        | Ap1(dir, ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Ap1(dir, ctx, d2) |> rewrap;
        | Ap2(dir, d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Ap2(dir, d1, ctx) |> rewrap;
        | TypAp(ctx, ty) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          TypAp(ctx, ty) |> rewrap;
        | DeferredAp1(ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          DeferredAp1(ctx, d2) |> rewrap;
        | DeferredAp2(d1, ctx, ds) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          DeferredAp2(d1, ctx, ds) |> rewrap;
        | If1(ctx, d2, d3) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          If1(ctx, d2, d3) |> rewrap;
        | If2(d1, ctx, d3) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          If2(d1, ctx, d3) |> rewrap;
        | If3(d1, d2, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          If3(d1, d2, ctx) |> rewrap;
        | UnOp(op, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          UnOp(op, ctx) |> rewrap;
        | BinOp1(op, ctx, d1) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          BinOp1(op, ctx, d1) |> rewrap;
        | BinOp2(op, d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          BinOp2(op, d1, ctx) |> rewrap;
        | TupleExtension1(ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          TupleExtension1(ctx, d2) |> rewrap;
        | TupleExtension2(d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          TupleExtension2(d1, ctx) |> rewrap;
        | Tuple(ctx, ds) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Tuple(ctx, ds) |> rewrap;
        | TupLabel(label, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          TupLabel(label, ctx) |> rewrap;
        | Dot1(ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Dot1(ctx, d2) |> rewrap;
        | Dot2(d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Dot2(d1, ctx) |> rewrap;
        | Test(ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Test(ctx) |> rewrap;
        | HintedTest(ctx, hint) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          HintedTest(ctx, hint) |> rewrap;
        | Parens(ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Parens(ctx) |> rewrap;
        | ListLit(ctx, ds) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          ListLit(ctx, ds) |> rewrap;
        | Cons1(ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Cons1(ctx, d2) |> rewrap;
        | Cons2(d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Cons2(d1, ctx) |> rewrap;
        | ListConcat1(ctx, d2) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          ListConcat1(ctx, d2) |> rewrap;
        | ListConcat2(d1, ctx) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          ListConcat2(d1, ctx) |> rewrap;
        | MultiHole(ctx, (dl, dr)) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          MultiHole(ctx, (dl, dr)) |> rewrap;
        | Asc(ctx, ty) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          Asc(ctx, ty) |> rewrap;
        | DynamicErrorHole(ctx, error) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          DynamicErrorHole(ctx, error) |> rewrap;
        | MatchScrut(ctx, rs) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          MatchScrut(ctx, rs) |> rewrap;
        | MatchRule(scr, p, ctx, rs) =>
          let+ ctx = walk(env, flt, ctx, exp, s);
          MatchRule(scr, p, ctx, rs) |> rewrap;
        };
      };
    let rctx =
      switch (residue) {
      | None => rctx
      | Some((level, act)) =>
        EvalCtx.Term({
          term: Filter(Residue(level, act), rctx),
          ids: [Id.mk()],
        })
      };
    (result, rctx);
  };
  let initial: State.t = {
    act,
    act_lvl: idx,
    ins_lvl: 0,
  };
  let scoped_filters = List.map(f => (f, env), flt);
  let (result, ctx) = walk(env, scoped_filters, ctx, exp, initial);
  (result.act, result.act_lvl, ctx);
};

let should_hide_eval_obj =
    (~settings, ~env, x: EvalObj.t): (FilterAction.action, EvalObj.t) => {
  let (act, idx, ctx) =
    matches(
      ~complete_filter=x.knd == CompleteFilter,
      env,
      [],
      x.ctx,
      x.d_loc,
      (Step, One),
      0,
    );
  let x = {
    ...x,
    ctx,
  };
  switch (act) {
  | (Eval, _) => (Eval, x)
  // Skip over Ascription
  | (Step, _) when step_kind_is_unrenderable(~settings, x.knd) => (Eval, x)
  | (Step, _) when idx > 0 => (Step, x)
  | (Step, _) when should_hide_step_kind(~settings, x.knd) => (Eval, x)
  | (Step, _) => (Step, x)
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
      EV_MODE with type inner_result = Result.t and type result = Result.t;
  } = {
    type requirement('a) = (Result.t, 'a);
    type requirements('a, 'b) = ('b, Result.t, Environment.t(Exp.t), 'a);
    type result = Result.t;
    type inner_result = result;

    let (&&&): (Result.t, Result.t) => Result.t =
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
        | Result.Indet => Result.Indet
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
          (r1 &&& r2, [v, ...vs]);
        };

    let req_all_final = (cont, wr, ds) => {
      req_all_final'(cont, wr, [], ds);
    };

    let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
      (rq, rl) => {
        let (undo, r, env, v) = rq;
        let rq_steps =
          switch (r) {
          | Result.Step(ss) => ss
          | Result.BoxedValue
          | Result.Indet => []
          };
        switch (rl(v)) {
        | Constructor => r
        | Value => List.is_empty(rq_steps) ? Result.BoxedValue : r
        | Indet => List.is_empty(rq_steps) ? Result.Indet : r
        | Step(s) when s.kind == CompleteFilter && !List.is_empty(rq_steps) =>
          Result.Step(rq_steps)
        | Step(s) =>
          Result.Step([EvalObj.mk(Mark, env, undo, s.kind), ...rq_steps])
        // TODO: Actually show these exceptions to the user!
        | exception (EvaluatorError.Exception(_)) => Result.Indet
        };
      };

    let (and.):
      (requirements('a, 'c => 'b), requirement('c)) =>
      requirements(('a, 'c), 'b) =
      ((u, r1, env, v1), (r2, v2)) => (u(v2), r1 &&& r2, env, (v1, v2));

    let otherwise = (env, o) => (o, Result.BoxedValue, env, ());
  };

  module Decomp = Transition(DecomposeEVMode);
  let rec decompose = (~in_closure=?, env, exp) => {
    switch (exp) {
    | _ =>
      Decomp.transition(
        decompose,
        ~mode=`Substitution,
        ~in_closure?,
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
        type inner_result = option(DHExp.t) and type result = option(DHExp.t);
  } = {
    type requirement('a) = 'a;
    type requirements('a, 'b) = 'a;
    type result = option(DHExp.t);
    type inner_result = result;

    // Assume that everything is either value or final as required.
    let req_final = (_, _, d) => d;
    let req_all_final = (_, _, ds) => ds;

    let (let.) = (rq: requirements('a, DHExp.t), rl: 'a => rule) =>
      switch (rl(rq)) {
      | Step({expr, _}) => Some(expr)
      | Constructor
      | Value
      | Indet => None
      };

    let (and.) = (x1, x2) => (x1, x2);

    let otherwise = (_, _) => ();
  };

  module TakeStepEV = Transition(TakeStepEVMode);

  let take_step = (~in_closure=?, env, d) =>
    TakeStepEV.transition(
      (~in_closure as _=?, _, _) => None,
      ~mode=`Substitution,
      ~in_closure?,
      env,
      d,
    )
    |> Option.map(DHExp.replace_all_ids);
};

let take_step = TakeStep.take_step;

let decompose = (d: DHExp.t, env: Environment.t(Exp.t)) => {
  let rs = Decompose.decompose(env, d);
  Decompose.Result.unbox(rs) |> List.rev;
};

/* ========== PUBLIC METHODS ========== */
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = EvalObj.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = EvalObj.persistent;

let persist = EvalObj.persist;

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | AutoStep(step)
  | AvailableSteps(list(step));

let get_status = (~settings: CoreSettings.t, exp, env) => {
  let eos =
    decompose(exp, env)
    |> List.map(should_hide_eval_obj(~settings=settings.evaluation, ~env)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
  switch (List.find_opt(((x, _)) => x == FilterAction.Eval, eos)) {
  | Some((_, x)) => AutoStep(x)
  | None => AvailableSteps(List.map(((_, x)) => x, eos))
  };
};

let get_step_id = (step: step): Id.t => step.d_loc |> DHExp.rep_id;

let get_step_kind = (step: step): step_kind => step.knd;

let take_step = (step: EvalObj.t) => {
  let+ next_expr = take_step(step.env, step.d_loc);
  let next_expr = {
    ...next_expr,
    annotation:
      IdTagged.IdTag.{
        ids: step.d_loc |> IdTagged.ids,
        secondary: empty_secondary,
        incomplete: [],
        lexeme: None,
      },
  };
  let next_expr =
    EvalCtx.compose(step.ctx, next_expr)
    |> Substitution.in_exp(Environment.empty)
    |> Exp.replace_all_ids;
  next_expr;
};

let refresh_step =
    (
      ~settings: CoreSettings.t,
      exp: Exp.t,
      env: Environment.t(Exp.t),
      step: persistent,
    ) => {
  let eos =
    decompose(exp, env)
    |> List.map(should_hide_eval_obj(~settings=settings.evaluation, ~env)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
  let* found = ProofHacks.nth_exp(step.at_exp, step.exp_idx, exp);
  let desired_id = IdTagged.ids(found);
  List.find_opt(
    ((_, step': step)) => IdTagged.ids(step'.d_loc) == desired_id,
    eos,
  );
};
