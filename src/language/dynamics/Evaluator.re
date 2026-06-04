open Transition;
open Trampoline.Syntax;

module EvaluatorEVMode: {
  type status =
    | Final
    | Uneval;

  include
    EV_MODE with
      type result =
        Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
} = {
  type status =
    | Final
    | Uneval;

  type result =
    Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
  type requirement('a) = Trampoline.t('a);
  type requirements('a, 'b) = Trampoline.t(('a, 'b));

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

type evaluate_result = (
  EvaluatorEVMode.status,
  list(EvaluatorState.effect),
  DHExp.t,
  EvaluatorState.t,
);

let rec evaluate =
        (
          ~reuse_map: IncrEval.reuse_map,
          ~prev: EvaluatorState.incr_eval=IncrEval.empty,
          ~reused_ids: Id.Map.t(unit),
          ~info_map: EvalInfo.t,
          ~in_closure=?,
          ~call_stack: CallStack.t',
          ~env,
          exp: DHExp.t,
          ~parent_state: ref(EvaluatorState.t),
          ~outbox: option(ref(IncrEval.t(EvaluatorState.t)))=?,
        )
        : Trampoline.t(DHExp.t) => {
  /* NOTE: This trampoline looks like it only returns an expression, but
   * it also mutates the eval_state and outbox references while it's
   * running. This is a bit of a hack, but it's necessary because the
   * trampoline is used to implement the incremental evaluation algorithm. */

  let expr_id = DHExp.rep_id(exp);

  /* OPTIMIZATION: If we're at a top level expression, we need to collect a
   * separate state for incremental evaluation, and later merge it. If we are
   * not at a top level expression, we can just add it directly to the parent's
   * state. */
  let is_top_level = call_stack.stack == [];
  let eval_state =
    is_top_level
      ? ref(EvaluatorState.empty_at(parent_state^.step_count)) : parent_state;
  let current_state = () => eval_state^;
  let set_current_state = (new_state: EvaluatorState.t) =>
    eval_state := new_state;

  /* If we did collect a separate state, we need to merge it into the parent state at the end */
  let update_parent =
    is_top_level
      ? () =>
          parent_state := EvaluatorState.append(parent_state^, eval_state^)
      : (() => ());

  switch (
    IncrEval.reuse_check(
      ~call_stack,
      ~prev,
      ~reuse_map,
      ~info_map,
      ~id=expr_id,
    )
  ) {
  | Some(entry) =>
    // Evaluation cache hit: reuse previous result
    eval_state := EvaluatorState.append(eval_state^, entry.state);
    // Add the entry to the next incremental evaluation cache
    eval_state := EvaluatorState.add_incr_entry(eval_state^, expr_id, entry);
    // Copy cache entries for every sub-id of the reused subtree from prev
    let f_exp = (continue, e: Exp.t): Exp.t => {
      let sub_id = Exp.rep_id(e);
      if (!Id.equal(sub_id, expr_id)) {
        switch (Id.Map.find_opt(sub_id, prev.entries)) {
        | Some(sub_entry) =>
          eval_state :=
            EvaluatorState.add_incr_entry(eval_state^, sub_id, sub_entry)
        | None => ()
        };
      };
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, entry.prev_elab);
    // Return
    update_parent();
    Trampoline.return(entry.value);
  | None =>
    // Evaluation cache miss: evaluate the expression from scratch
    let current_step_count = current_state().step_count;

    // If this expression is a probe target, record the probe start
    let call_stack =
      switch (Id.Map.find_opt(expr_id, info_map.targets)) {
      | Some(_) =>
        CallStack.record_probe_start(call_stack, expr_id, current_step_count)
      | None => call_stack
      };

    let eval_core = (): Trampoline.t(evaluate_result) => {
      let evaluate_child = (~in_closure=?, env, child) => {
        let.trampoline (status, effects, value, fragment) =
          evaluate(
            ~outbox?,
            ~reuse_map,
            ~prev,
            ~info_map,
            ~reused_ids,
            ~in_closure?,
            ~call_stack,
            state,
            env,
            child,
          );
        if (!use_ref) {
          eval_state := EvaluatorState.append(eval_state^, fragment);
        };
        Trampoline.return((status, effects, value));
      };
      let.trampoline (is_finished, effects, next) =
        Eval.transition(
          (~in_closure=?, env, init) =>
            evaluate_child(~in_closure?, env, init),
          ~mode=`Environment,
          ~targets=info_map.targets,
          ~in_closure?,
          env,
          init,
        );

      /* If this expression is in the targets and evaluation is complete,
       * emit RecordExpProbe effect */
      let effects =
        switch (is_finished, Id.Map.find_opt(expr_id, info_map.targets)) {
        | (Final, Some(pr)) => [
            EvaluatorState.RecordExpProbe(pr),
            ...effects,
          ]
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
        EvaluatorState.update(
          info_map,
          current_state(),
          call_stack,
          env,
          init,
          next,
          effects,
        );
      set_current_state(new_state);

      /* Binder body provenance map: RecordPatMatch describes `pat <- rhs`.
       * We add pattern provenance only when the rhs value came from the
       * previous cache. Otherwise the binding shadows any outer provenance
       * for those names and dependents must be recalculated.
       *
       * Function bodies are not incremental-cache boundaries: we do not record
       * entries while inside a call stack, and reuse_check also refuses reuse
       * there. In probe-enabled runs, keep the old path because probe capture
       * shares this evaluation plumbing and relies on the fully threaded maps. */
      let body_reuse_map =
        if (call_stack.stack != [] && Id.Map.is_empty(info_map.targets)) {
          reuse_map;
        } else {
          List.fold_left(
            (reuse_map, effect) =>
              switch (effect) {
              | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
                let source_id = DHExp.rep_id(rhs);
                IncrEval.update_maps_after_binding(
                  ~rhs_reused=Id.Map.mem(source_id, reused_ids),
                  ~source_id,
                  pat,
                  ~reuse_map,
                );
              | _ => reuse_map
              },
            reuse_map,
            effects,
          );
        };

      switch (is_finished) {
      | Final =>
        Trampoline.return((
          EvaluatorEVMode.Final,
          [],
          next,
          if (use_ref) {
            empty_fragment();
          } else {
            eval_state^;
          },
        ))
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
        switch (Id.Map.find_opt(expr_id, info_map.targets)) {
        | Some(probe) =>
          let.trampoline (_, _, final_value, child_fragment) =
            Trampoline.Next(
              () =>
                evaluate(
                  ~outbox?,
                  ~reuse_map=body_reuse_map,
                  ~prev,
                  ~info_map,
                  ~reused_ids,
                  ~in_closure?,
                  ~call_stack,
                  state,
                  env,
                  next,
                ),
            );
          if (!use_ref) {
            eval_state := EvaluatorState.append(eval_state^, child_fragment);
          };
          let step_start =
            CallStack.get_probe_start(call_stack, expr_id)
            |> Option.value(~default=0);
          let step_end = current_state().step_count - 1;
          let args =
            CallStack.lookup_app_arg(
              call_stack,
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
          let _ = CallStack.clear_probe_start(call_stack, expr_id);
          set_current_state(
            EvaluatorState.add_sample(current_state(), sample),
          );
          Trampoline.return((
            EvaluatorEVMode.Final,
            [],
            final_value,
            if (use_ref) {
              empty_fragment();
            } else {
              eval_state^;
            },
          ));
        | None =>
          let.trampoline (status, effects, final_value, child_fragment) =
            Trampoline.Next(
              () =>
                evaluate(
                  ~outbox?,
                  ~reuse_map=body_reuse_map,
                  ~reused_ids,
                  ~prev,
                  ~info_map,
                  ~in_closure?,
                  ~call_stack,
                  state,
                  env,
                  next,
                ),
            );
          if (!use_ref) {
            eval_state := EvaluatorState.append(eval_state^, child_fragment);
          };
          Trampoline.return((
            status,
            effects,
            final_value,
            if (use_ref) {
              empty_fragment();
            } else {
              eval_state^;
            },
          ));
        }
      };
    };

    // Record incremental entry if required
    let info_snapshot =
      if (call_stack.stack != []) {
        None;
      } else {
        EvalInfo.find_opt(expr_id, info_map);
      };
    switch (info_snapshot) {
    | None => eval_core()
    | Some({
        elab_term: prev_elab,
        co_ctx,
        probe_targets: prev_probe_targets,
        _,
      }) =>
      let.trampoline (status, effects, final, fragment) = eval_core();
      let entry: IncrEval.entry(EvaluatorState.t) = {
        prev_elab,
        prev_reuse_map:
          IncrEval.make_clean(
            IncrEval.restrict_to_co_ctx(reuse_map, co_ctx),
          ),
        prev_probe_targets,
        value: final,
        state: fragment,
      };
      switch (outbox) {
      | Some(outbox) => outbox := IncrEval.add_entry(expr_id, entry, outbox^)
      | None => ()
      };
      state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
      state := EvaluatorState.append(state^, fragment);
      Trampoline.return((status, effects, final, empty_fragment()));
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

let evaluate_and_limit =
    (
      ~step_limit: int,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~info_map: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
      d: DHExp.t,
    )
    : limited_result => {
  let state = ref(EvaluatorState.empty);
  let reused_ids =
    Id.Map.map(
      _ => (),
      ReusePass.reuse_pass(~prev, ~info_map, ~env, ~reuse_map, d).entries,
    );
  let result =
    evaluate(
      ~prev,
      ~info_map,
      ~call_stack=CallStack.empty,
      ~reuse_map,
      ~reused_ids,
      state,
      env,
      d,
    );
  let result =
    Trampoline.Yielding.run_slice(
      ~step_budget=step_limit,
      result |> Trampoline.Yielding.start,
    );
  switch (result) {
  | SliceDone((_, _, x, fragment)) =>
    state := EvaluatorState.append(state^, fragment);
    LimitedCompleted((
      x |> Substitution.in_exp(env) |> Exp.replace_all_ids,
      state^,
    ));
  | SliceYielded(_) => StepLimitExceeded
  };
};

type yielding_evaluation = {
  env: Environment.t(Exp.t),
  state: ref(EvaluatorState.t),
  outbox: ref(IncrEval.t(EvaluatorState.t)),
  continuation: Trampoline.Yielding.continuation(evaluate_result),
};

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

let start_yielding_evaluation =
    (
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~info_map: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
      d: DHExp.t,
    )
    : yielding_evaluation => {
  let state = ref(EvaluatorState.empty);
  let outbox = ref(IncrEval.empty);
  let reused_ids =
    Id.Map.map(
      _ => (),
      ReusePass.reuse_pass(~prev, ~info_map, ~env, ~reuse_map, d).entries,
    );
  let result =
    evaluate(
      ~outbox,
      ~prev,
      ~info_map,
      ~call_stack=CallStack.empty,
      ~reuse_map,
      ~reused_ids,
      state,
      env,
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
    (evaluation: yielding_evaluation): IncrEval.t(EvaluatorState.t) => {
  let outbox = evaluation.outbox^;
  evaluation.outbox := IncrEval.empty;
  outbox;
};

let run_yielding_slice =
    (~step_budget: int, evaluation: yielding_evaluation): yielding_result =>
  switch (
    Trampoline.Yielding.run_slice(~step_budget, evaluation.continuation)
  ) {
  | SliceDone((_, _, x, fragment)) =>
    evaluation.state := EvaluatorState.append(evaluation.state^, fragment);
    EvaluationCompleted((
      x |> Substitution.in_exp(evaluation.env) |> Exp.replace_all_ids,
      evaluation.state^,
    ));
  | SliceYielded(continuation) =>
    EvaluationYielded({
      ...evaluation,
      continuation,
    })
  };

let evaluate =
    (
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~info_map: EvalInfo.t=EvalInfo.empty,
      ~env,
      d: DHExp.t,
    )
    : (Exp.t, EvaluatorState.t) => {
  let state = ref(EvaluatorState.empty);
  let reuse_map = IncrEval.clean_reuse_map_of_env(env);
  let reused_ids =
    Id.Map.map(
      _ => (),
      ReusePass.reuse_pass(~prev, ~info_map, ~env, ~reuse_map, d).entries,
    );
  let result =
    evaluate(
      ~prev,
      ~info_map,
      ~call_stack=CallStack.empty,
      ~reuse_map,
      ~reused_ids,
      state,
      env,
      d,
    );
  let (_, _, e, fragment) = Trampoline.run(result);
  state := EvaluatorState.append(state^, fragment);
  (e |> Substitution.in_exp(env) |> Exp.replace_all_ids, state^);
};
