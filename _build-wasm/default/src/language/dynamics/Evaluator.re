open Transition;
open Trampoline.Syntax;

module EvaluatorEVMode: {
  type status =
    | Final
    | Uneval;

  include
    EV_MODE with
      type inner_result = Trampoline.t(DHExp.t) and
      type result =
        Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
} = {
  type status =
    | Final
    | Uneval;

  type inner_result = Trampoline.t(DHExp.t);
  type result =
    Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
  type requirement('a) = Trampoline.t('a);
  type requirements('a, 'b) = Trampoline.t(('a, 'b));

  let req_final = (f, _, x) => {
    let.trampoline x = Next(() => f(x));
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
        // Constants
        (
          ~prev: EvaluatorState.incr_eval=IncrEval.empty,
          ~track_reuse: bool,
          ~reused_ids: Id.Map.t(unit),
          ~eval_info: EvalInfo.t,
          // Call Stack
          ~in_closure=?,
          ~call_stack: CallStack.state,
          // Inputs
          ~reuse_map: IncrEval.reuse_map,
          env,
          exp: DHExp.t,
          // Outputs
          ~parent_state: ref(EvaluatorState.t),
          ~outbox: option(ref(IncrEval.outbox(EvaluatorState.t))),
          ~current_top_id: option(Id.t),
        )
        : Trampoline.t(DHExp.t) => {
  /* NOTE: This trampoline looks like it only returns an expression, but
   * it also mutates the parent_state and outbox references while it's
   * running. */

  let evaluate =
    evaluate(~prev, ~track_reuse, ~reused_ids, ~eval_info, ~outbox);
  let expr_id = DHExp.rep_id(exp);
  /* Only key outbox.current by ids from the elaborated program.
   * Stepped intermediates use Id.invalid (targets empty / probes off) or
   * fresh UUIDs (probes on). Publishing under those ids either collides with
   * StreamCollector's own Exp.temp nodes (truncating the walk so streamed
   * results appear to go backwards) or never matches the walk. Keeping a
   * prior program id across intermediates is also wrong: eval_5 gives temps
   * a fresh empty inner_state, and publishing that under the kept id makes
   * the collector short-circuit with an empty/partial state. Leave current
   * untouched instead (current_top_id = None ⇒ no outbox write) so the last
   * real-id publish remains until the next program node. */
  let current_top_id =
    if (call_stack.stack == []) {
      switch (EvalInfo.find_opt(expr_id, eval_info)) {
      | Some(_) => Some(expr_id)
      | None => None
      };
    } else {
      current_top_id;
    };
  let replay_state = (state: EvaluatorState.t): EvaluatorState.t => {
    ...state,
    incr_eval: IncrEval.empty,
  };
  let update_outbox_current = (state: EvaluatorState.t) =>
    switch (outbox, current_top_id) {
    | (Some(outbox), Some(id)) =>
      outbox :=
        IncrEval.set_outbox_current(~id, ~state=replay_state(state), outbox^)
    | (None, _)
    | (_, None) => ()
    };

  // Fully evaluate all children and take this expression one step forward
  let eval_0_main =
      (~reuse_map, ~in_closure=?, ~call_stack, ~state, env, exp: DHExp.t)
      : EvaluatorEVMode.result => {
    Eval.transition(
      (~in_closure=?, env, child) =>
        evaluate(
          ~reuse_map,
          ~in_closure?,
          ~call_stack,
          ~parent_state=state,
          ~current_top_id,
          env,
          child,
        ),
      ~mode=`Environment,
      ~targets=eval_info.targets,
      ~in_closure?,
      env,
      exp,
    );
  };

  // Do the above but also run side effects on state and stack
  let eval_1_effects =
      (~reuse_map, ~in_closure=?, ~call_stack, ~state, env, exp: DHExp.t) => {
    let.trampoline (is_finished, effects, next) =
      eval_0_main(~reuse_map, ~in_closure?, ~call_stack, ~state, env, exp);

    let (call_stack, new_state) =
      EvaluatorState.update(eval_info, state^, call_stack, env, exp, effects);

    state := new_state;
    update_outbox_current(state^);

    /* Function bodies are not incremental-cache boundaries: we do not record
     * entries while inside a call stack, and reuse_check also refuses reuse
     * there. Skip entirely when nothing downstream can consume the map. */
    let body_reuse_map =
      if (!track_reuse || call_stack.stack != []) {
        reuse_map;
      } else {
        ReusePass.update_reuse_map_after_effects(
          ~rhs_reused=source_id => Id.Map.mem(source_id, reused_ids),
          ~reuse_map,
          effects,
        );
      };

    Trampoline.return((is_finished, call_stack, body_reuse_map, next));
  };

  // Do the above but until the expression is final
  let eval_2_until_final =
      (~reuse_map, ~in_closure=?, ~call_stack, ~state, env, exp: DHExp.t) => {
    let.trampoline (is_finished, call_stack, body_reuse_map, next) =
      eval_1_effects(~reuse_map, ~in_closure?, ~call_stack, ~state, env, exp);

    switch (is_finished) {
    | Final => Trampoline.return((next, call_stack))
    | Uneval =>
      let.trampoline final_value =
        Trampoline.Next(
          () =>
            evaluate(
              ~reuse_map=body_reuse_map,
              ~in_closure?,
              ~call_stack,
              ~parent_state=state,
              ~current_top_id,
              env,
              next,
            ),
        );
      Trampoline.return((final_value, call_stack));
    };
  };

  // Do the above but also record probe samples if required
  let eval_3_record_probe_sample =
      (
        ~call_stack,
        ~state: ref(EvaluatorState.t),
        ~expr_id,
        env,
        exp: DHExp.t,
      ) => {
    let current_step_count = state^.step_count;

    /* Save original call_stack before update. For probed compound expressions
     * (Uneval case), we need this because:
     * - The updated call_stack (after RecordStackFrame) should be passed to
     *   recursive evaluation so inner expressions see the app_id
     * - But the probe sample for THIS expression should use the original
     *   call_stack (what it was before entering the function) */
    let original_call_stack = call_stack;

    let.trampoline (final_value, probe_call_stack) =
      eval_2_until_final(
        ~reuse_map,
        ~in_closure?,
        ~call_stack,
        ~state,
        env,
        exp,
      );

    // Record probe sample if required
    switch (Id.Map.find_opt(expr_id, eval_info.targets)) {
    | Some(probe) =>
      let step_start = current_step_count;
      let step_end = state^.step_count - 1;
      let args =
        CallStack.lookup_app_arg(
          probe_call_stack,
          expr_id,
          original_call_stack.stack,
        );
      let sample =
        Sample.mk(
          ~args,
          ~step_start,
          ~step_end,
          expr_id,
          final_value,
          env,
          original_call_stack.stack,
          probe,
        );
      state := EvaluatorState.add_sample(state^, sample);
      update_outbox_current(state^);
    | None => ()
    };

    Trampoline.return(final_value);
  };

  // Do the above but also reuse the previous result if possible
  let eval_4_reuse =
      (
        ~call_stack: CallStack.state,
        ~state: ref(EvaluatorState.t),
        ~expr_id,
        env,
        exp: DHExp.t,
      )
      : Trampoline.t(DHExp.t) => {
    switch (
      IncrEval.reuse_check(
        ~call_stack,
        ~prev,
        ~reuse_map,
        ~eval_info,
        ~id=expr_id,
      )
    ) {
    | Some(entry) =>
      // Evaluation cache hit: reuse previous result
      state := EvaluatorState.append(state^, entry.state);
      update_outbox_current(state^);
      // Add the entry to the next incremental evaluation cache
      state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
      // Copy cache entries for every sub-id of the reused subtree from prev
      state :=
        {
          ...state^,
          incr_eval:
            IncrEval.copy_descendant_entries(
              ~root_id=expr_id,
              ~root=entry.prev_elab,
              ~prev,
              state^.incr_eval,
            ),
        };
      Trampoline.return(entry.value);
    | None =>
      // Evaluation cache miss: evaluate the expression from scratch
      let.trampoline final_value =
        eval_3_record_probe_sample(~call_stack, ~state, ~expr_id, env, exp);

      // Record incremental entry if required
      let info_snapshot =
        if (call_stack.stack != []) {
          None;
        } else {
          EvalInfo.find_opt(expr_id, eval_info);
        };
      switch (info_snapshot) {
      | None => Trampoline.return(final_value)
      | Some({
          elab_term: prev_elab,
          co_ctx,
          probe_targets: prev_probe_targets,
          _,
        }) =>
        let entry: IncrEval.entry(EvaluatorState.t) = {
          prev_elab,
          prev_reuse_map:
            IncrEval.make_clean(
              IncrEval.restrict_to_co_ctx(reuse_map, co_ctx),
            ),
          prev_probe_targets,
          value: final_value,
          state: replay_state(state^),
        };

        switch (outbox) {
        | Some(outbox) =>
          outbox := IncrEval.add_outbox_entry(expr_id, entry, outbox^)
        | None => ()
        };
        state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
        Trampoline.return(final_value);
      };
    };
  };

  // [PERF] We collect separate states for top-level expressions so we can replay those states.
  let eval_5_state_merge =
      (~call_stack: CallStack.state, ~state, ~expr_id, env, exp) =>
    if (call_stack.stack == []) {
      let inner_state =
        ref(EvaluatorState.empty_at(parent_state^.step_count));
      let.trampoline final_value =
        eval_4_reuse(~call_stack, ~state=inner_state, ~expr_id, env, exp);
      let new_state = EvaluatorState.append(state^, inner_state^);
      state :=
        {
          ...new_state,
          incr_eval:
            IncrEval.add_stream(inner_state^.incr_eval, new_state.incr_eval),
        };
      update_outbox_current(inner_state^);
      Trampoline.return(final_value);
    } else {
      eval_4_reuse(~call_stack, ~state, ~expr_id, env, exp);
    };

  eval_5_state_merge(~call_stack, ~state=parent_state, ~expr_id, env, exp);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

let finish = (~env, e: DHExp.t): Exp.t =>
  e |> Substitution.in_exp(env) |> Exp.replace_all_ids;

/* Shared setup for all evaluation entry points: run the reuse pass to find
 * reusable cache entries, then build the (unstarted) evaluation trampoline. */
let prepare_evaluation =
    (
      ~prev,
      ~eval_info: EvalInfo.t,
      ~env,
      ~reuse_map: option(IncrEval.reuse_map),
      ~outbox,
      d: DHExp.t,
    )
    : (ref(EvaluatorState.t), Trampoline.t(DHExp.t)) => {
  /* The reuse map is only ever consumed by reuse_check or by incr-entry
   * snapshots, both of which need statics in eval_info (reuse_check also
   * needs a non-empty prev). When neither can fire — e.g. `hazel run`,
   * MVU app dispatch — skip maintaining it: the per-binder
   * remove_pat_bindings walk dominates evaluation otherwise. */
  let track_reuse =
    !IncrEval.is_empty(prev) || EvalInfo.has_statics(eval_info);
  let reuse_map =
    switch (reuse_map) {
    | Some(m) => m
    | None =>
      track_reuse
        ? IncrEval.clean_reuse_map_of_env(env) : IncrEval.empty_reuse_map
    };
  let state = ref(EvaluatorState.empty);
  /* The pre-pass only yields entries via reuse_check, which needs both a
   * non-empty prev and statics; otherwise it is a full walk of the program
   * for a guaranteed-empty result. */
  let reused_ids =
    IncrEval.is_empty(prev) || !EvalInfo.has_statics(eval_info)
      ? Id.Map.empty
      : Id.Map.map(
          _ => (),
          ReusePass.reuse_pass(~prev, ~eval_info, ~env, ~reuse_map, d).
            entries,
        );
  let result =
    evaluate(
      ~prev,
      ~track_reuse,
      ~eval_info,
      ~call_stack=CallStack.empty,
      ~reuse_map,
      ~reused_ids,
      ~parent_state=state,
      ~outbox,
      ~current_top_id=None,
      env,
      d,
    );
  (state, result);
};

let evaluate_and_limit =
    (
      ~step_limit: int,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: option(IncrEval.reuse_map)=?,
      ~outbox: option(ref(IncrEval.outbox(EvaluatorState.t)))=?,
      d: DHExp.t,
    )
    : limited_result => {
  let (state, result) =
    prepare_evaluation(~prev, ~eval_info, ~env, ~reuse_map, ~outbox, d);
  switch (
    Trampoline.Yielding.run_slice(
      ~step_budget=step_limit,
      result |> Trampoline.Yielding.start,
    )
  ) {
  | SliceDone(x) => LimitedCompleted((finish(~env, x), state^))
  | SliceYielded(_) => StepLimitExceeded
  };
};

type yielding_evaluation = {
  env: Environment.t(Exp.t),
  state: ref(EvaluatorState.t),
  outbox: ref(IncrEval.outbox(EvaluatorState.t)),
  continuation: Trampoline.Yielding.continuation(DHExp.t),
};

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

let start_yielding_evaluation =
    (
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: option(IncrEval.reuse_map)=?,
      d: DHExp.t,
    )
    : yielding_evaluation => {
  let outbox = ref(IncrEval.empty_outbox);
  let (state, result) =
    prepare_evaluation(
      ~prev,
      ~eval_info,
      ~env,
      ~reuse_map,
      ~outbox=Some(outbox),
      d,
    );
  {
    env,
    state,
    outbox,
    continuation: Trampoline.Yielding.start(result),
  };
};

let drain_streaming_outbox =
    (evaluation: yielding_evaluation): IncrEval.outbox(EvaluatorState.t) => {
  let outbox = evaluation.outbox^;
  evaluation.outbox := IncrEval.empty_outbox;
  outbox;
};

let run_yielding_slice =
    (~step_budget: int, evaluation: yielding_evaluation): yielding_result =>
  switch (
    Trampoline.Yielding.run_slice(~step_budget, evaluation.continuation)
  ) {
  | SliceDone(x) =>
    EvaluationCompleted((finish(~env=evaluation.env, x), evaluation.state^))
  | SliceYielded(continuation) =>
    EvaluationYielded({
      ...evaluation,
      continuation,
    })
  };

let yielding_step_count = (evaluation: yielding_evaluation): int => {
  let Trampoline.Yielding.Continuation(_, _, step_counter) =
    evaluation.continuation;
  step_counter;
};

let evaluate =
    (
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      d: DHExp.t,
    )
    : (Exp.t, EvaluatorState.t) => {
  let (state, result) =
    prepare_evaluation(
      ~prev,
      ~eval_info,
      ~env,
      ~reuse_map=None,
      ~outbox=None,
      d,
    );
  /* Must be sequenced before reading `state`: running the trampoline is what
     populates it, and tuple components are evaluated right-to-left. */
  let value = finish(~env, Trampoline.run(result));
  (value, state^);
};
