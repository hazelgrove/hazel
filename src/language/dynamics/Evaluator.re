open Transition;

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

// This module defines the stack machine for the evaluator.
module Trampoline = {
  type t('a) =
    | Bind(t('b), 'b => t('a)): t('a)
    | Next(unit => t('a)): t('a)
    | Done('a): t('a);

  type callstack('a, 'b) =
    | Finished: callstack('a, 'a)
    | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);
  let rec run:
    type a b.
      (~step_limit: int=?, ~step_counter: int=?, t(b), callstack(b, a)) =>
      step_constrained(a) =
    (
      ~step_limit: option(int)=?,
      ~step_counter=0,
      t: t(b),
      callstack: callstack(b, a),
    ) => {
      switch (step_limit) {
      | Some(x) when x <= step_counter => StepLimitExceeded
      | _ =>
        switch (t) {
        | Bind(t, f) =>
          run(
            ~step_limit?,
            ~step_counter=step_counter + 1,
            t,
            Continue(f, callstack),
          )
        | Next(f) =>
          run(~step_limit?, ~step_counter=step_counter + 1, f(), callstack)
        | Done(x) =>
          switch (callstack) {
          | Finished => Completed(x)
          | Continue(f, callstack) =>
            run(
              ~step_limit?,
              ~step_counter=step_counter + 1,
              f(x),
              callstack,
            )
          }
        }
      };
    };

  let run = (~step_limit: option(int)=?, t) =>
    run(~step_limit?, t, Finished);

  let return = x => Done(x);

  let bind = (t, f) => Bind(t, f);

  module Syntax = {
    let (let.trampoline) = (x, f) => bind(x, f);
  };
};

module EvaluatorEVMode: {
  type status =
    | Final
    | Uneval;

  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and
      type result =
        Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
} = {
  open Trampoline.Syntax;

  type status =
    | Final
    | Uneval;

  type result =
    Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
  type requirement('a) = Trampoline.t('a);
  type requirements('a, 'b) = Trampoline.t(('a, 'b));

  type state = ref(EvaluatorState.t);

  let req_final = (f, _, x) => {
    let.trampoline (_, _, x) = Next(() => f(x));
    Trampoline.return(x);
  };
  let rec req_all_final = (f, i, xs) =>
    switch (xs) {
    | [] => Trampoline.return([])
    | [x, ...xs] =>
      let.trampoline x' = req_final(f, x => x, x);
      let.trampoline xs' = req_all_final(f, i, xs);
      Trampoline.return([x', ...xs']);
    };

  let otherwise = (_, c) => Trampoline.return(((), c));
  let (and.) = (t1, t2) => {
    let.trampoline (x1, c1) = t1;
    let.trampoline x2 = t2;
    Trampoline.return(((x1, x2), c1(x2)));
  };
  let (let.) = (t1, s) => {
    let.trampoline (x, c) = t1;
    switch (s(x)) {
    | Step({expr, side_effects, is_value: true, _}) =>
      Trampoline.return((Final, side_effects, expr))
    | Step({expr, side_effects, is_value: false, _}) =>
      Trampoline.return((Uneval, side_effects, expr))
    | Constructor
    | Value
    | Indet => Trampoline.return((Final, [], c))
    };
  };
};

module Eval = Transition(EvaluatorEVMode);

let rec evaluate =
        (
          ~in_closure=?,
          ~call_stack: Sample.call_stack,
          state: EvaluatorEVMode.state,
          env,
          init: DHExp.t,
        )
        : EvaluatorEVMode.result => {
  open Trampoline.Syntax;

  /* Check if this expression is in the targets.
   * If so, record the current step count before evaluation begins. */
  let expr_id = DHExp.rep_id(init);
  switch (Id.Map.find_opt(expr_id, state^.targets)) {
  | Some(_) => state := EvaluatorState.record_probe_start(state^, expr_id)
  | None => ()
  };

  let.trampoline (is_finished, effects, next) =
    Eval.transition(
      (~in_closure=?, env, init) =>
        evaluate(~in_closure?, ~call_stack, state, env, init),
      ~mode=`Environment,
      ~targets=state^.targets,
      ~in_closure?,
      env,
      init,
    );

  /* If this expression is in the targets and evaluation is complete,
   * emit RecordExpProbe effect */
  let effects =
    switch (is_finished, Id.Map.find_opt(expr_id, state^.targets)) {
    | (Final, Some(pr)) => [EvaluatorState.RecordExpProbe(pr), ...effects]
    | _ => effects
    };

  /* Save original call_stack before update. For probed compound expressions
   * (Uneval case), we need this because:
   * - The updated call_stack (after RecordStackFrame) should be passed to
   *   recursive evaluation so inner expressions see the app_id
   * - But the probe sample for THIS expression should use the original
   *   call_stack (what it was before entering the function) */
  let original_call_stack = call_stack;
  let (call_stack, new_state) =
    EvaluatorState.update(state^, call_stack, env, init, next, effects);
  state := new_state;
  switch (is_finished) {
  | Final => Trampoline.return((EvaluatorEVMode.Final, [], next))
  | Uneval =>
    /* Compound Expression Probe Capture via Trampoline.Bind
     *
     * Problem: Compound expressions (if, let, case, function application) step
     * with is_finished=Uneval, meaning their result is a new expression with a
     * different ID. Without special handling, we'd call evaluate(next) and lose
     * the probe context since next.id != expr_id.
     *
     * Example: ^^probe(if true then 1 else 2)
     *   1. expr_id = ID of the if expression, which is in targets
     *   2. transition returns (Uneval, effects, next=1) - If stepped to branch
     *   3. Without Bind: evaluate(1) runs, returns Final, but expr_id is lost
     *   4. With Bind: we capture the final value when evaluate(1) completes,
     *      then record the sample with the original expr_id
     *
     * Nested probes like ^^probe(if true then ^^probe(1) else 2) work correctly:
     * each probe creates its own Bind continuation, and they're unwound in order.
     * Trampoline.Bind creates a continuation that runs AFTER all recursive
     * evaluation completes, at which point state^ reflects all step count
     * mutations, but we still have expr_id in scope.
     *
     * Important: We use original_call_stack for the probe sample (the call_stack
     * before RecordStackFrame), but call_stack (the updated one) for recursive
     * evaluation. This ensures:
     * - ^^probe(f(x)) records a sample with the call_stack BEFORE entering f
     * - Expressions inside f see the app_id of f(x) in their call_stacks
     */
    switch (Id.Map.find_opt(expr_id, state^.targets)) {
    | Some(probe) =>
      let.trampoline (_, _, final_value) =
        Trampoline.Next(() => evaluate(~call_stack, state, env, next));
      /* Record sample with final evaluated value, using original_call_stack */
      let step_start =
        EvaluatorState.get_probe_start(state^, expr_id)
        |> Option.value(~default=0);
      let step_end = state^.step_count - 1;
      /* Look up arg if this probe is on an Ap expression */
      let args =
        EvaluatorState.lookup_app_arg(state^, expr_id, original_call_stack);
      let sample =
        Sample.mk(
          ~args,
          ~step_start,
          ~step_end,
          expr_id,
          final_value,
          env,
          original_call_stack,
          probe,
        );
      state := EvaluatorState.clear_probe_start(state^, expr_id);
      state :=
        {
          ...state^,
          probes: Sample.Map.extend(expr_id, sample, state^.probes),
        };
      Trampoline.return((EvaluatorEVMode.Final, [], final_value));
    | None => Trampoline.Next(() => evaluate(~call_stack, state, env, next))
    }
  };
};

let evaluate_and_limit =
    (
      ~step_limit: option(int)=?,
      ~targets: Sample.targets=Sample.no_targets,
      ~env,
      d: DHExp.t,
    )
    : step_constrained((Exp.t, EvaluatorState.t)) => {
  let state = ref(EvaluatorState.mk(~targets));
  let result = evaluate(~call_stack=[], state, env, d);
  let result = Trampoline.run(~step_limit?, result);
  switch (result) {
  | Completed((_, _, x)) =>
    Completed((x |> Substitution.in_exp(env) |> Exp.replace_all_ids, state^))
  | StepLimitExceeded => StepLimitExceeded
  };
};

let evaluate =
    (~targets: Sample.targets=Sample.no_targets, ~env, d: DHExp.t)
    : (Exp.t, EvaluatorState.t) => {
  switch (evaluate_and_limit(~targets, ~env, d)) {
  | Completed((x, state)) => (x, state)
  | StepLimitExceeded =>
    raise(Failure("Impossible: Step limit exceeded when not set"))
  };
};
