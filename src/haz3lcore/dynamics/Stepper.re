open Sexplib.Std;
open EvaluatorStep;
open Transition;

exception Exception;

type step_with_previous = {
  step,
  previous: option(step),
  hidden: list(step),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type current =
  | StepperOK(DHExp.t, EvaluatorState.t)
  | StepTimeout // Must have at least one in previous
  | StepPending(DHExp.t, EvaluatorState.t, option(EvalObj.t)); // StepPending(_,Some(_)) cannot be saved

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* Might be different to first expression in previous because
     steps are taken automatically (this may no longer be true - Matt) */
  elab: DHExp.t,
  previous: list(step),
  current,
  next: list((FilterAction.action, EvalObj.t)),
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
    | Module1(d1, ctx, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Module1(d1, ctx, d3);
    | Module2(d1, d2, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Module2(d1, d2, ctx);
    | Dot1(ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Dot1(ctx, d2);
    | Dot2(d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Dot2(d1, ctx);
    | Fun(dp, ty, ctx, name) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Fun(dp, ty, ctx, name);
    | FixF(name, ty, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      FixF(name, ty, ctx);
    | TypAp(ctx, ty) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      TypAp(ctx, ty);
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

let get_elab = ({elab, _}: t) => elab;

let get_next_steps = s => s.next;

let current_expr = (s: t) =>
  switch (s.current, s.previous) {
  | (StepperOK(d, _), _)
  | (StepPending(d, _, _), _) => d
  | (StepTimeout, [x, ..._]) => x.d
  | (StepTimeout, []) => s.elab
  };

let step_pending = (idx: int, {elab, previous, current, next}: t) => {
  // TODO[Matt]: change to nth_opt after refactor
  let eo = List.nth(next, idx) |> snd;
  switch (current) {
  | StepperOK(d, s) => {
      elab,
      previous,
      current: StepPending(d, s, Some(eo)),
      next,
    }
  | StepTimeout => {
      elab,
      previous: List.tl(previous),
      current:
        StepPending(
          List.hd(previous).d,
          List.hd(previous).state,
          Some(eo),
        ),
      next,
    }
  | StepPending(d, s, _) => {
      elab,
      previous,
      current: StepPending(d, s, Some(eo)),
      next,
    }
  };
};

let init = (~settings, elab: DHExp.t) => {
  {
    elab,
    previous: [],
    current: StepPending(elab, EvaluatorState.init, None),
    next: decompose(~settings, elab),
  };
};

let rec evaluate_pending = (~settings, s: t) => {
  switch (s.current) {
  | StepperOK(_)
  | StepTimeout => s
  | StepPending(d, state, Some(eo)) =>
    let state_ref = ref(state);
    let d_loc' =
      switch (take_step(state_ref, eo.env, eo.d_loc)) {
      | Some(d) => d
      | None => raise(Exception)
      };
    let d' = compose(eo.ctx, d_loc');
    {
      elab: s.elab,
      previous: [
        {d, d_loc: eo.d_loc, ctx: eo.ctx, knd: eo.knd, state},
        ...s.previous,
      ],
      current: StepPending(d', state_ref^, None),
      next: decompose(~settings, d'),
    }
    |> evaluate_pending(~settings);
  | StepPending(d, state, None) =>
    switch (List.find_opt(((act, _)) => act == FilterAction.Eval, s.next)) {
    | Some((_, eo)) =>
      {
        elab: s.elab,
        previous: s.previous,
        current: StepPending(d, state, Some(eo)),
        next: s.next,
      }
      |> evaluate_pending(~settings)
    | None => {
        elab: s.elab,
        previous: s.previous,
        current: StepperOK(d, state),
        next: s.next,
      }
    }
  };
};

let rec evaluate_full = (~settings, s: t) => {
  switch (s.current) {
  | StepTimeout => s
  | StepperOK(_) when s.next == [] => s
  | StepperOK(_) => s |> step_pending(0) |> evaluate_full(~settings)
  | StepPending(_) =>
    evaluate_pending(~settings, s) |> evaluate_full(~settings)
  };
};

let timeout =
  fun
  | {elab, previous, current: StepPending(d, state, Some(eo)), next} => {
      elab,
      previous: [
        {d, d_loc: eo.d_loc, ctx: eo.ctx, knd: eo.knd, state},
        ...previous,
      ],
      current: StepTimeout,
      next,
    }
  | {current: StepTimeout | StepperOK(_) | StepPending(_, _, None), _} as s => s;

// let rec step_forward = (~settings, e: EvalObj.t, s: t) => {
//   let current = compose(e.ctx, e.apply());
//   skip_steps(
//     ~settings,
//     {
//       current,
//       previous: [{d: s.current, step: e}, ...s.previous],
//       next: decompose(current),
//     },
//   );
// }
// and skip_steps = (~settings, s) => {
//   switch (
//     List.find_opt(
//       (x: EvalObj.t) => should_hide_step(~settings, x.knd),
//       s.next,
//     )
//   ) {
//   | None => s
//   | Some(e) => step_forward(~settings, e, s)
//   };
// };

let rec undo_point =
        (~settings): (list(step) => option((step, list(step)))) =>
  fun
  | [] => None
  | [x, ...xs] when should_hide_step(~settings, x) |> fst == Eval =>
    undo_point(~settings, xs)
  | [x, ...xs] => Some((x, xs));

let step_backward = (~settings, s: t) =>
  switch (undo_point(~settings, s.previous)) {
  | None => failwith("cannot step backwards")
  | Some((x, xs)) => {
      current: StepperOK(x.d, x.state),
      next: decompose(~settings, x.d),
      previous: xs,
      elab: s.elab,
    }
  };

let get_justification: step_kind => string =
  fun
  | LetBind => "substitution"
  | ModuleBind => "module substitution"
  | Sequence => "sequence"
  | FixUnwrap => "unroll fixpoint"
  | UpdateTest => "update test"
  | TypFunAp => "apply type function"
  | FunAp => "apply function"
  | BuiltinWrap => "wrap builtin"
  | BuiltinAp(s) => "evaluate " ++ s
  | BinIntOp(Plus | Minus | Times | Power | Divide)
  | BinFloatOp(Plus | Minus | Times | Power | Divide) => "arithmetic"
  | BinIntOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | BinFloatOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) => "comparison"
  | BinIntOp(Equals | NotEquals)
  | BinFloatOp(Equals | NotEquals)
  | BinStringOp(Equals) => "check equality"
  | BinStringOp(Concat) => "string manipulation"
  | BinBoolOp(_) => "boolean logic"
  | Conditional(_) => "conditional"
  | ListCons => "list manipulation"
  | ListConcat => "list manipulation"
  | CaseApply => "case selection"
  | CaseNext => "case discarding"
  | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
  | InvalidStep => "error"
  | VarLookup => "variable lookup"
  | ModuleLookup => "module lookup"
  | DotAccess => "access member"
  | CastTypAp
  | CastAp
  | Cast => "cast calculus"
  | FixClosure => "fixpoint closure"
  | CompleteFilter => "complete filter"
  | CompleteClosure => "complete closure"
  | FunClosure => "function closure"
  | Skip => "skipped steps";

let get_history = (~settings, stepper) => {
  let rec get_history':
    list(step) => (list(step), list(step_with_previous)) =
    fun
    | [] => ([], [])
    | [step, ...steps] => {
        let (hidden, ss) = get_history'(steps);
        switch (step |> should_hide_step(~settings) |> fst) {
        | Eval => ([step, ...hidden], ss)
        | Step => (
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
          )
        };
      };
  stepper.previous |> get_history';
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = {
  elab: DHExp.t,
  previous: list(step),
  current,
};

let (sexp_of_persistent, persistent_of_sexp) =
  StructureShareSexp.structure_share_in(
    sexp_of_persistent,
    persistent_of_sexp,
  );

// Remove EvalObj.t objects from stepper to prevent problems when loading
let to_persistent: t => persistent =
  fun
  | {elab, previous, current: StepPending(d, state, Some(_)), _} => {
      elab,
      previous,
      current: StepPending(d, state, None),
    }
  | {elab, previous, current, _} => {elab, previous, current};

let from_persistent = (~settings, {elab, previous, current}) => {
  let s = {elab, previous, current, next: []};
  {elab, previous, current, next: decompose(~settings, current_expr(s))};
};
