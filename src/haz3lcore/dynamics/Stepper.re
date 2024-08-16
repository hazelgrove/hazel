open EvaluatorStep;
open Transition;
open Util;

exception Exception;

[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_state =
  | StepPending(int)
  | StepperReady
  | StepperDone
  | StepTimeout(EvalObj.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type history = Aba.t((DHExp.t, EvaluatorState.t), step);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  history,
  next_options: list((FilterAction.action, EvalObj.t)),
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
      let rewrap = term => EvalCtx.Term({term, ids});
      switch ((term: EvalCtx.term)) {
      | Closure(env, ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Closure(env, ctx) |> rewrap;
      | Filter(Filter(flt'), ctx) =>
        let flt = flt |> FilterEnvironment.extends(flt');
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Filter(Filter(flt'), ctx) |> rewrap;
      | Filter(Residue(idx', act'), ctx) =>
        let (ract, ridx, rctx) =
          if (idx > idx') {
            matches(env, flt, ctx, exp, act, idx);
          } else {
            matches(env, flt, ctx, exp, act', idx');
          };
        if (act' |> snd == All) {
          (ract, ridx, Filter(Residue(idx', act'), rctx) |> rewrap);
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
      | Fun(dp, ctx, env', name) =>
        let+ ctx =
          matches(Option.value(~default=env, env'), flt, ctx, exp, act, idx);
        Fun(dp, ctx, env', name) |> rewrap;
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
      | Test(ctx) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        Test(ctx) |> rewrap;
      | HintedTest(ctx, h) =>
        let+ ctx = matches(env, flt, ctx, exp, act, idx);
        HintedTest(ctx, h) |> rewrap;
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

let get_elab = ({history, _}: t): Elaborator.Elaboration.t => {
  let (d, _) = Aba.last_a(history);
  {d: d};
};

let get_next_steps = s => s.next_options |> List.map(snd);

let current_expr = ({history, _}: t) => Aba.hd(history) |> fst;

let current_state = ({history, _}: t) => Aba.hd(history) |> snd;

let step_pending = (idx: int, stepper: t) => {
  {...stepper, stepper_state: StepPending(idx)};
};

let init = (~settings, {d}: Elaborator.Elaboration.t) => {
  let state = EvaluatorState.init;
  {
    history: Aba.singleton((d, state)),
    next_options: decompose(~settings, d, state),
    stepper_state: StepperReady,
  };
};

let rec evaluate_pending = (~settings, s: t) => {
  switch (s.stepper_state) {
  | StepperDone
  | StepTimeout(_) => s
  | StepperReady =>
    let next' = List.mapi((i, x) => (i, x), s.next_options);
    switch (
      List.find_opt(((_, (act, _))) => act == FilterAction.Eval, next')
    ) {
    | Some((i, (_, _))) =>
      {...s, stepper_state: StepPending(i)} |> evaluate_pending(~settings)
    | None => {...s, stepper_state: StepperDone}
    };
  | StepPending(i) =>
    let (_, eo) = List.nth(s.next_options, i);
    let (d, state) = Aba.hd(s.history);
    let state_ref = ref(state);
    let d_loc' =
      (
        switch (take_step(state_ref, eo.env, eo.d_loc)) {
        | Some(d) => d |> DHExp.repair_ids
        | None => raise(Exception)
        }
      )
      |> DHExp.repair_ids;
    let _ = print_endline(d_loc' |> DHExp.show);
    let d' = EvalCtx.compose(eo.ctx, d_loc');
    let new_step = {
      d,
      d_loc: eo.d_loc,
      d_loc',
      ctx: eo.ctx,
      knd: eo.knd,
      state,
    };
    let new_state = state_ref^;
    {
      history: s.history |> Aba.cons((d', new_state), new_step),
      stepper_state: StepperReady,
      next_options: decompose(~settings, d', new_state),
    }
    |> evaluate_pending(~settings);
  };
};

let rec evaluate_full = (~settings, s: t) => {
  switch (s.stepper_state) {
  | StepTimeout(_) => s
  | StepperDone when s.next_options == [] => s
  | StepperDone => s |> step_pending(0) |> evaluate_full(~settings)
  | StepperReady
  | StepPending(_) =>
    evaluate_pending(~settings, s) |> evaluate_full(~settings)
  };
};

let timeout =
  fun
  | {stepper_state: StepPending(idx), _} as s => {
      ...s,
      stepper_state: StepTimeout(List.nth(s.next_options, idx) |> snd),
    }
  | {stepper_state: StepTimeout(_) | StepperReady | StepperDone, _} as s => s;

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
    next_options:
      decompose(~settings, Aba.hd(h') |> fst, Aba.hd(h') |> snd),
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
  | TypFunAp => "apply type function"
  | FunAp => "apply function"
  | DeferredAp => "deferred application"
  | BuiltinWrap => "wrap builtin"
  | BuiltinAp(s) => "evaluate " ++ s
  | UnOp(Int(Minus))
  | BinIntOp(Plus | Minus | Times | Power | Divide)
  | BinFloatOp(Plus | Minus | Times | Power | Divide) => "arithmetic"
  | BinIntOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | BinFloatOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) => "comparison"
  | BinIntOp(Equals | NotEquals)
  | BinFloatOp(Equals | NotEquals)
  | BinStringOp(Equals) => "check equality"
  | BinStringOp(Concat) => "string manipulation"
  | UnOp(Bool(Not))
  | BinBoolOp(_) => "boolean logic"
  | Conditional(_) => "conditional"
  | ListCons => "list manipulation"
  | ListConcat => "list manipulation"
  | CaseApply => "case selection"
  | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
  | InvalidStep => "error"
  | VarLookup => "variable lookup"
  | CastTypAp
  | CastAp
  | Cast => "cast calculus"
  | FixClosure => "fixpoint closure"
  | CompleteFilter => "complete filter"
  | CompleteClosure => "complete closure"
  | FunClosure => "function closure"
  | RemoveTypeAlias => "define type"
  | RemoveParens => "remove parentheses"
  | UnOp(Meta(Unquote)) => failwith("INVALID STEP");

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

let hidden_steps_of_info = (info: step_info): list(step_info) => {
  // note the previous_step field is fudged because it is currently usused.next_options
  List.map(
    ((hs: step, _)) =>
      {
        d: hs.d,
        chosen_step: Some(hs),
        hidden_steps: [],
        previous_step: None,
      },
    info.hidden_steps,
  );
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = {history};

let (sexp_of_persistent, persistent_of_sexp) =
  StructureShareSexp.structure_share_in(
    sexp_of_persistent,
    persistent_of_sexp,
  );

let (sexp_of_persistent, persistent_of_sexp) =
  StructureShareSexp.structure_share_in(
    sexp_of_persistent,
    persistent_of_sexp,
  );

// Remove EvalObj.t objects from stepper to prevent problems when loading
let to_persistent: t => persistent = ({history, _}) => {history: history};

let from_persistent = (~settings, {history}) => {
  {
    history,
    next_options:
      decompose(~settings, Aba.hd(history) |> fst, Aba.hd(history) |> snd),
    stepper_state: StepperDone,
  };
};
