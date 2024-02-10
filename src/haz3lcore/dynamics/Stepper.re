open Sexplib.Std;
open EvaluatorStep;
open Transition;
open Util;

exception Exception;

[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_state =
  | StepPending(EvalObj.t)
  | StepperReady
  | StepperDone
  | StepperError(EvalObj.t, ProgramEvaluatorError.t)
  | StepTimeout(EvalObj.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type history = Aba.t((DHExp.t, EvaluatorState.t), step);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  history,
  next_options: list(EvalObj.t),
  stepper_state,
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
    | Filter(_, _) => (pact, pidx)
    | _ => midx > idx ? (mact, midx) : (pact, pidx)
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
    | Filter(Residue(idx, act), ctx) =>
      let (ract, ridx, rctx) = matches(env, flt, ctx, exp, act, idx);
      if (ridx == idx && ract |> snd == All) {
        (ract, ridx, Filter(Residue(idx, act), rctx));
      } else {
        (ract, ridx, rctx);
      };
    | Seq1(ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Seq1(ctx, d2);
    | Seq2(d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Seq2(d1, ctx);
    | Let1(d1, ctx, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Let1(d1, ctx, d3);
    | Let2(d1, d2, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Let2(d1, d2, ctx);
    | Fun(dp, ty, ctx, env', name) =>
      let+ ctx =
        matches(Option.value(~default=env, env'), flt, ctx, exp, act, idx);
      Fun(dp, ty, ctx, env', name);
    | FixF(name, ty, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      FixF(name, ty, ctx);
    | Ap1(dir, ctx, d2) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Ap1(dir, ctx, d2);
    | Ap2(dir, d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      Ap2(dir, d1, ctx);
    | If1(c, ctx, d2, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      If1(c, ctx, d2, d3);
    | If2(c, d1, ctx, d3) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      If2(c, d1, ctx, d3);
    | If3(c, d1, d2, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      If3(c, d1, d2, ctx);
    | BinOp1(op, ctx, d1) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinOp1(op, ctx, d1);
    | BinOp2(op, d1, ctx) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      BinOp2(op, d1, ctx);
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
    | MatchScrut(c, ctx, rs) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      MatchScrut(c, ctx, rs);
    | MatchRule(c, scr, p, ctx, rs) =>
      let+ ctx = matches(env, flt, ctx, exp, act, idx);
      MatchRule(c, scr, p, ctx, rs);
    };
  switch (ctx) {
  | Filter(_) => (ract, ridx, rctx)
  | _ when midx == ridx && midx > pidx && mact |> snd == All => (
      ract,
      ridx,
      Filter(Residue(midx, mact), rctx),
    )
  | _ => (ract, ridx, rctx)
  };
};

let should_hide_eval_obj =
    (~settings, x: EvalObj.t): (FilterAction.action, EvalObj.t) =>
  if (should_hide_step(~settings, x.knd)) {
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
  if (should_hide_step(~settings, x.knd)) {
    (Eval, x);
  } else {
    let (act, _, ctx) =
      matches(ClosureEnvironment.empty, [], x.ctx, x.d_loc, (Step, One), 0);
    switch (act) {
    | (Eval, _) => (Eval, {...x, ctx})
    | (Step, _) => (Step, {...x, ctx})
    };
  };

let get_elab = ({history, _}: t) => Aba.last_a(history) |> fst;

let get_next_steps = s => s.next_options;

let current_expr = ({history, _}: t) => Aba.hd(history);

let step_pending = (eo: EvalObj.t, stepper: t) => {
  ...stepper,
  stepper_state: StepPending(eo),
};

let init = (elab: DHExp.t) => {
  {
    history: Aba.singleton((elab, EvaluatorState.init)),
    next_options: decompose(elab),
    stepper_state: StepperReady,
  };
};

let rec evaluate_pending = (~settings, s: t) => {
  switch (s.stepper_state) {
  | StepperDone
  | StepperError(_)
  | StepTimeout(_) => s
  | StepperReady =>
    let next' = s.next_options |> List.map(should_hide_eval_obj(~settings));
    switch (List.find_opt(((act, _)) => act == FilterAction.Eval, next')) {
    | Some((_, eo)) =>
      {...s, stepper_state: StepPending(eo)} |> evaluate_pending(~settings)
    | None => {...s, stepper_state: StepperDone}
    };
  | StepPending(eo) =>
    let (d, state) = Aba.hd(s.history);
    let state_ref = ref(state);
    let d_loc' =
      (
        switch (take_step(state_ref, eo.env, eo.d_loc)) {
        | Some(d) => d
        | None => raise(Exception)
        }
      )
      |> DHExp.repair_ids;
    let d' = EvalCtx.compose(eo.ctx, d_loc');
    let new_step = {
      d,
      d_loc: eo.d_loc,
      d_loc',
      ctx: eo.ctx,
      knd: eo.knd,
      state,
    };
    {
      history: s.history |> Aba.cons((d', state_ref^), new_step),
      stepper_state: StepperReady,
      next_options: decompose(d'),
    }
    |> evaluate_pending(~settings);
  };
};

let rec evaluate_full = (~settings, s: t) => {
  switch (s.stepper_state) {
  | StepperError(_)
  | StepTimeout(_) => s
  | StepperDone when s.next_options == [] => s
  | StepperDone =>
    s |> step_pending(List.hd(s.next_options)) |> evaluate_full(~settings)
  | StepperReady
  | StepPending(_) =>
    evaluate_pending(~settings, s) |> evaluate_full(~settings)
  };
};

let timeout =
  fun
  | {stepper_state: StepPending(eo), _} as s => {
      ...s,
      stepper_state: StepTimeout(eo),
    }
  | {
      stepper_state:
        StepperError(_) | StepTimeout(_) | StepperReady | StepperDone,
      _,
    } as s => s;

let rec truncate_history = (~settings) =>
  fun
  | ([_, ...as_], [b, ...bs])
      when should_hide_step(~settings, b) |> fst == Eval =>
    truncate_history(~settings, (as_, bs))
  | ([_, ...as_], [_, ...bs]) => Some((as_, bs))
  | _ => None;

let step_backward = (~settings, s: t) => {
  let h' =
    truncate_history(~settings, s.history)
    |> Option.value(~default=s.history);
  {
    history: h',
    next_options: decompose(Aba.hd(h') |> fst),
    stepper_state: StepperDone,
  };
};

let can_undo = (~settings, s: t) => {
  truncate_history(~settings, s.history) |> Option.is_some;
};

let get_justification: step_kind => string =
  fun
  | LetBind => "substitution"
  | Seq => "sequence"
  | FixUnwrap => "unroll fixpoint"
  | UpdateTest => "update test"
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
  | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
  | InvalidStep => "error"
  | VarLookup => "variable lookup"
  | CastAp
  | Cast => "cast calculus"
  | CompleteFilter => "unidentified step"
  | CompleteClosure => "unidentified step"
  | FunClosure => "unidentified step"
  | Skip => "skipped steps";

type step_info = {
  d: DHExp.t,
  chosen_step: option(step), // The step that was taken next
  hidden_steps: list((step, Id.t)), // The hidden steps between previous_step and the current one (an Id in included because it may have changed since the step was taken)
  previous_step: option((step, Id.t)) // The step that will be displayed above this one (an Id in included because it may have changed since the step was taken)
};

let get_history = (~settings, stepper) => {
  let should_skip_step = step =>
    step |> should_hide_step(~settings) |> fst == Eval;
  let grouped_steps =
    stepper.history
    |> Aba.fold_right(
         ((d, _), step, result) =>
           if (should_skip_step(step)) {
             Aba.map_hd(((_, hs)) => (d, [step, ...hs]), result);
           } else {
             Aba.cons((d, []), step, result);
           },
         ((d, _)) => Aba.singleton((d, [])),
       );
  let replace_id = (x, y, (s, z)) => (s, x == z ? y : z);
  let track_ids =
      (
        (
          chosen_step: option(step),
          (d: DHExp.t, hidden_steps: list(step)),
          previous_step: option(step),
        ),
      ) => {
    let (previous_step, hidden_steps) =
      List.fold_left(
        ((ps, hs), h: step) => {
          let replacement =
            replace_id(h.d_loc |> DHExp.rep_id, h.d_loc' |> DHExp.rep_id);
          (
            Option.map(replacement, ps),
            [(h, h.d_loc' |> DHExp.rep_id), ...List.map(replacement, hs)],
          );
        },
        (Option.map(x => (x, x.d_loc' |> DHExp.rep_id), previous_step), []),
        hidden_steps,
      );
    {d, previous_step, hidden_steps, chosen_step};
  };
  let padded = grouped_steps |> Aba.bab_triples;
  let result = padded |> List.map(track_ids);
  result;
  //grouped_steps |> Aba.bab_triples |> List.map(track_ids);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = {history};

// Remove EvalObj.t objects from stepper to prevent problems when loading
let to_persistent: t => persistent = ({history, _}) => {history: history};

let from_persistent: persistent => t =
  ({history}) => {
    {
      history,
      next_options: decompose(Aba.hd(history) |> fst),
      stepper_state: StepperDone,
    };
  };
