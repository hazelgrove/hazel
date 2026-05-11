open Transition;

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

// This module defines the stack machine for the evaluator.
module Trampoline = {
  /* Trampoline terms.
   *
   *   Bind(t, f)              — run t, pass result to f
   *   Next(thunk)             — defer thunk to next runner step
   *   Done(x)                 — resolve to x
   *   WithBudget(b, fb, t)    — run t under a budget of `b` trampoline
   *                             steps; if the runner exceeds the budget
   *                             before t resolves, the body is aborted
   *                             and `fb` is propagated up as t's result.
   *                             Fires only inside the dynamic extent of
   *                             this constructor, so concurrently-active
   *                             nested budgets each fire independently.
   */
  type t('a) =
    | Bind(t('b), 'b => t('a)): t('a)
    | Next(unit => t('a)): t('a)
    | Done('a): t('a)
    | WithBudget(int, 'a, t('a)): t('a);

  /* Callstack frames during run.
   *
   *   Finished                  — top-level; consumes the final value
   *   Continue(f, k)            — inner Bind continuation
   *   Marker(deadline, fb, k)   — guards a WithBudget body. `deadline`
   *                               is an absolute step_counter value;
   *                               if step_counter exceeds it before the
   *                               body produces Done, the runner aborts
   *                               by replacing the body's result with
   *                               `fb` (same type as the body's value).
   *                               On clean completion, the marker is a
   *                               no-op pass-through to k. */
  type callstack('a, 'b) =
    | Finished: callstack('a, 'a)
    | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c)
    | Marker(int, 'a, callstack('a, 'b)): callstack('a, 'b);

  /* Walk a callstack to find the smallest active marker deadline.
   * Used to maintain a single-int "next deadline" the runner can check
   * once per step instead of re-walking the stack. */
  let rec callstack_min_deadline: type a b. callstack(a, b) => int =
    cs =>
      switch (cs) {
      | Finished => max_int
      | Continue(_, k) => callstack_min_deadline(k)
      | Marker(d, _, k) => min(d, callstack_min_deadline(k))
      };

  /* Result of "abort to the topmost-exceeded marker": the new term to
   * resume with (Done(fallback)) and the marker's parent callstack. The
   * existential 'a is the body type at that marker's level — it must
   * still match the parent's input type, which the GADT enforces. */
  type abort_result('b) =
    | Aborted(t('a), callstack('a, 'b)): abort_result('b);

  /* Walk the callstack top-to-bottom; abort to the first marker whose
   * deadline has been exceeded. (Innermost-exceeded; if multiple have
   * fired, the inner one fires first locally, and the next step
   * re-evaluates the outer.) */
  let rec find_exceeded_marker:
    type a b. (int, callstack(a, b)) => option(abort_result(b)) =
    (now, cs) =>
      switch (cs) {
      | Finished => None
      | Continue(_, parent) => find_exceeded_marker(now, parent)
      | Marker(d, fb, parent) when d < now =>
        Some(Aborted(Done(fb), parent))
      | Marker(_, _, parent) => find_exceeded_marker(now, parent)
      };

  /* A paused trampoline computation: the current term plus the callstack
   * needed to resume it. The inner type variable is existential — the
   * runner's chunk loop (run_chunk) hides it behind chunk_result. */
  type suspended('a) =
    | Susp(t('b), callstack('b, 'a), int /* min_deadline */): suspended('a);

  /* Result of running a bounded chunk of work. `Suspended` carries the
   * frozen continuation; resume by passing it back to run_chunk_resume.
   * `StepLimitExceeded` is the same global cap as before; `Completed`
   * is the final result. */
  type chunk_result('a) =
    | Completed('a)
    | Suspended(suspended('a))
    | StepLimitExceeded;

  /* Step the trampoline at most `step_budget` times before yielding.
   * This is the resumable core; see `run` below for the non-resumable
   * convenience wrapper.
   *
   * `min_deadline` is the smallest active marker deadline; threaded
   * through to keep the per-step abort check O(1) (just an int compare)
   * in the no-markers / not-yet-exceeded case. It's recomputed on
   * Marker push/pop and on abort. */
  let rec run_chunk:
    type a b.
      (
        ~step_limit: int=?,
        ~step_budget: int,
        ~step_counter: int=?,
        ~min_deadline: int=?,
        t(b),
        callstack(b, a)
      ) =>
      chunk_result(a) =
    (
      ~step_limit: option(int)=?,
      ~step_budget,
      ~step_counter=0,
      ~min_deadline=max_int,
      t: t(b),
      callstack: callstack(b, a),
    ) =>
      if (step_counter >= step_budget) {
        Suspended(Susp(t, callstack, min_deadline));
      } else {
        switch (step_limit) {
        | Some(x) when x <= step_counter => StepLimitExceeded
        | _ =>
          /* Per-marker abort: cheap int compare in the common case;
           * we only walk the stack when something is provably overdue. */
          if (step_counter > min_deadline) {
            switch (find_exceeded_marker(step_counter, callstack)) {
            | Some(Aborted(t', cs')) =>
              run_chunk(
                ~step_limit?,
                ~step_budget,
                ~step_counter=step_counter + 1,
                ~min_deadline=callstack_min_deadline(cs'),
                t',
                cs',
              )
            | None =>
              /* Should be unreachable: min_deadline tracks the smallest
               * live marker, so step_counter > min_deadline implies one
               * exists. Defensively, treat as no-op. */
              run_chunk(
                ~step_limit?,
                ~step_budget,
                ~step_counter=step_counter + 1,
                ~min_deadline=max_int,
                t,
                callstack,
              )
            };
          } else {
            switch (t) {
            | Bind(t, f) =>
              run_chunk(
                ~step_limit?,
                ~step_budget,
                ~step_counter=step_counter + 1,
                ~min_deadline,
                t,
                Continue(f, callstack),
              )
            | Next(f) =>
              run_chunk(
                ~step_limit?,
                ~step_budget,
                ~step_counter=step_counter + 1,
                ~min_deadline,
                f(),
                callstack,
              )
            | WithBudget(budget, fallback, body) =>
              let deadline = step_counter + budget;
              run_chunk(
                ~step_limit?,
                ~step_budget,
                ~step_counter=step_counter + 1,
                ~min_deadline=
                  if (deadline < min_deadline) {
                    deadline;
                  } else {
                    min_deadline;
                  },
                body,
                Marker(deadline, fallback, callstack),
              );
            | Done(x) =>
              switch (callstack) {
              | Finished => Completed(x)
              | Continue(f, callstack) =>
                run_chunk(
                  ~step_limit?,
                  ~step_budget,
                  ~step_counter=step_counter + 1,
                  ~min_deadline,
                  f(x),
                  callstack,
                )
              | Marker(_, _, k) =>
                /* Body completed cleanly; pop the marker, recompute
                 * min_deadline (since one live marker just disappeared). */
                run_chunk(
                  ~step_limit?,
                  ~step_budget,
                  ~step_counter=step_counter + 1,
                  ~min_deadline=callstack_min_deadline(k),
                  Done(x),
                  k,
                )
              }
            };
          }
        };
      };

  /* Resume a previously-suspended computation for another budgeted chunk. */
  let resume_chunk =
      (~step_limit: option(int)=?, ~step_budget: int, Susp(t, cs, min_deadline))
      : chunk_result(_) =>
    run_chunk(~step_limit?, ~step_budget, ~min_deadline, t, cs);

  /* Top-level callstack value. `Finished` is a GADT constructor; binding
   * it as a value lets it cross the interface boundary. */
  let finished: type a. callstack(a, a) = Finished;

  /* Synchronous run-to-completion: drive `run_chunk` in big chunks and
   * collapse `Suspended` back into more chunks. Preserves the original
   * `run` API for callers that don't need cooperative scheduling
   * (CLI, main-thread eval, tests). */
  let run = (~step_limit: option(int)=?, t) => {
    let rec drive: chunk_result('a) => step_constrained('a) =
      fun
      | Completed(x) => Completed(x)
      | StepLimitExceeded => StepLimitExceeded
      | Suspended(s) =>
        drive(resume_chunk(~step_limit?, ~step_budget=max_int / 2, s));
    drive(run_chunk(~step_limit?, ~step_budget=max_int / 2, t, Finished));
  };

  let return = x => Done(x);

  let bind = (t, f) => Bind(t, f);

  /* Run `body` under an absolute budget of `budget` trampoline steps;
   * on overrun, the body's value is replaced with `fallback`. Side
   * effects performed before the abort are NOT unwound — the runner
   * just stops driving the body. */
  let with_budget = (~budget, ~fallback, body) =>
    WithBudget(budget, fallback, body);

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

/* Per-id timeout combinator. Wraps a body trampoline so that, if the
 * runner spends more than `budget` trampoline steps inside the body,
 * the body is aborted mid-flight and the result is replaced with
 * `Invalid("Timeout")` at id `id`. Side effects performed before the
 * abort persist (we don't unwind state mutations); only the body's
 * value is replaced.
 *
 * Implementation: emits a `Trampoline.WithBudget` term, which the
 * runner interprets by pushing a `Marker` frame. On every step the
 * runner compares `step_counter` against the smallest active marker
 * deadline; on overrun it walks back to the marker and resumes with
 * `Done(fallback)` as the body's value, dropping any work above.
 *
 * The cache-write at the bottom of `evaluate` independently checks for
 * `Invalid("Timeout")` and skips `add_incr_entry`, so a timed-out id
 * gets a fresh attempt on the next run. */
let with_id_budget =
    (
      ~id: Id.t,
      ~budget: int,
      ~state as _: EvaluatorEVMode.state,
      body: EvaluatorEVMode.result,
    )
    : EvaluatorEVMode.result => {
  let timeout_term: TermBase.Exp.term = Invalid("Timeout");
  let timeout_value = DHExp.mk([id], timeout_term);
  let fallback = (EvaluatorEVMode.Final, [], timeout_value);
  Trampoline.with_budget(~budget, ~fallback, body);
};

/* Default per-id budget. Set high enough that nothing in the standard
 * test suite hits it, but low enough that a runaway recursion on a
 * single subterm trips the per-id timeout instead of the cell-level
 * step_limit. The cell-level cap (see `WorkerServer.Sched.cell_step_limit`)
 * is the safety net for genuinely-infinite computations.
 *
 * Tests dial this lower via `with_id_budget` directly to exercise the
 * timeout path without standing up a worker. */
let default_per_id_budget = 50_000_000;

/* True if the (already-evaluated) result of an id should NOT be cached.
 * Currently: skip caching `Invalid("Timeout")` so the next run gets a
 * fresh chance at a real result. */
let should_skip_cache_for = (final: DHExp.t): bool =>
  switch (DHExp.term_of(final)) {
  | Invalid("Timeout") => true
  | _ => false
  };

let rec evaluate =
        (
          ~dirty_names: list(Var.t)=[],
          ~prev: IncrEval.t=IncrEval.empty,
          ~info_slice: IncrEval.InfoSlice.t=IncrEval.InfoSlice.empty,
          ~in_closure=?,
          ~call_stack: Sample.call_stack,
          state: EvaluatorEVMode.state,
          env,
          init: DHExp.t,
        )
        : EvaluatorEVMode.result => {
  open Trampoline.Syntax;

  let expr_id = DHExp.rep_id(init);

  let reuse =
    if (call_stack != [] || IncrEval.is_empty(prev)) {
      None;
    } else {
      IncrEval.reuse_check(
        ~prev,
        ~dirty_names,
        ~info_slice,
        ~current_targets=state^.targets,
        ~id=expr_id,
        ~curr_elab=init,
      );
    };

  switch (reuse) {
  | Some(entry) =>
    /* Replay slice side-effects (probes, tests, theorems, app_args,
     * step_count) so downstream UI sees the same trace a full re-run would. */
    state := EvaluatorState.replay_slice(entry.slice, state^);
    state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
    state := EvaluatorState.mark_incr_reused(state^, expr_id);
    Trampoline.return((EvaluatorEVMode.Final, [], entry.value));
  | None =>
    switch (Id.Map.find_opt(expr_id, state^.targets)) {
    | Some(_) => state := EvaluatorState.record_probe_start(state^, expr_id)
    | None => ()
    };

    /* Snapshot for slice capture and dirty-propagation comparison. Only
     * needed when we're going to record a cache entry for this id. */
    let elab_snapshot =
      if (call_stack != []) {
        None;
      } else {
        switch (IncrEval.InfoSlice.find_opt(expr_id, info_slice)) {
        | Some({elab_term, _}) => Some(elab_term)
        | None => None
        };
      };
    let state_before = state^;

    let eval_core = () => {
      let.trampoline (is_finished, effects, next) =
        Eval.transition(
          (~in_closure=?, env, init) =>
            evaluate(
              ~dirty_names,
              ~prev,
              ~info_slice,
              ~in_closure?,
              ~call_stack,
              state,
              env,
              init,
            ),
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
        EvaluatorState.update(state^, call_stack, env, init, next, effects);
      state := new_state;

      /* Binder body dirty set: any RecordPatMatch side-effects produced by
       * this transition describe a `pat <- rhs` binding. If the rhs produced
       * a value different from prev, the pattern's bound names become dirty
       * for the body only (siblings/outer scopes never see this extension).
       * Reading from side-effects (rather than switching on init.term) means
       * any future pat-binding construct that calls `matches` and emits
       * RecordPatMatch participates automatically. */
      let body_dirty_names =
        List.concat_map(
          fun
          | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
            IncrEval.newly_dirty_vars(~prev, ~curr=state^.incr_eval, pat, rhs)
          | _ => [],
          effects,
        )
        @ dirty_names;

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
            Trampoline.Next(
              () =>
                evaluate(
                  ~dirty_names=body_dirty_names,
                  ~prev,
                  ~info_slice,
                  ~call_stack,
                  state,
                  env,
                  next,
                ),
            );
          let step_start =
            EvaluatorState.get_probe_start(state^, expr_id)
            |> Option.value(~default=0);
          let step_end = state^.step_count - 1;
          let args =
            EvaluatorState.lookup_app_arg(
              state^,
              expr_id,
              original_call_stack,
            );
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
          state := EvaluatorState.add_sample(state^, sample);
          Trampoline.return((EvaluatorEVMode.Final, [], final_value));
        | None =>
          Trampoline.Next(
            () =>
              evaluate(
                ~dirty_names=body_dirty_names,
                ~prev,
                ~info_slice,
                ~call_stack,
                state,
                env,
                next,
              ),
          )
        }
      };
    };

    /* Per-id timeout: wrap the recursive body so an over-budget id
     * returns Invalid("Timeout"). See `with_id_budget` above. */
    let budgeted_core = () =>
      with_id_budget(
        ~id=expr_id,
        ~budget=default_per_id_budget,
        ~state,
        eval_core(),
      );

    switch (elab_snapshot) {
    | None => budgeted_core()
    | Some(prev_elab) =>
      /* Capture the slice once the whole subtree's evaluation resolves. */
      Trampoline.Bind(
        budgeted_core(),
        ((status, effects, final)) =>
          if (should_skip_cache_for(final)) {
            /* Don't cache a timed-out id; the next run gets to retry. */
            Trampoline.return((status, effects, final));
          } else {
            let slice =
              EvaluatorState.capture_slice(
                ~before=state_before,
                ~after=state^,
              );
            let entry: IncrEval.entry = {
              prev_elab,
              value: final,
              slice,
              targets_snapshot: state^.targets,
            };
            state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
            state :=
              EvaluatorState.mark_incr_recalculated(state^, expr_id);
            Trampoline.return((status, effects, final));
          },
      )
    };
  };
};

/* Public, scheduler-facing trampoline shape. The result is the raw DHExp
 * value the recursive evaluator returns; callers run substitution / replace
 * via `finalize_value` once they're ready. The state ref is exposed so
 * the worker scheduler can snapshot `state^.incr_eval` between chunks
 * (e.g. for partial-cache merging on restart). */
let evaluate_trampoline =
    (
      ~targets: Sample.targets=Sample.no_targets,
      ~prev: IncrEval.t=IncrEval.empty,
      ~info_slice: IncrEval.InfoSlice.t=IncrEval.InfoSlice.empty,
      ~env,
      d: DHExp.t,
    )
    : (ref(EvaluatorState.t), Trampoline.t(DHExp.t)) => {
  let state = ref(EvaluatorState.mk(~targets));
  let inner = evaluate(~prev, ~info_slice, ~call_stack=[], state, env, d);
  let mapped =
    Trampoline.Bind(inner, ((_, _, v)) => Trampoline.return(v));
  (state, mapped);
};

/* Apply substitution + replace_all_ids to land a DHExp value in
 * Exp.t form for downstream consumers. Counterpart to
 * `evaluate_trampoline`. */
let finalize_value = (~env, dh: DHExp.t): Exp.t =>
  dh |> Substitution.in_exp(env) |> Exp.replace_all_ids;

let evaluate_and_limit =
    (
      ~step_limit: option(int)=?,
      ~targets: Sample.targets=Sample.no_targets,
      ~prev: IncrEval.t=IncrEval.empty,
      ~info_slice: IncrEval.InfoSlice.t=IncrEval.InfoSlice.empty,
      ~env,
      d: DHExp.t,
    )
    : step_constrained((Exp.t, EvaluatorState.t)) => {
  let state = ref(EvaluatorState.mk(~targets));
  let result = evaluate(~prev, ~info_slice, ~call_stack=[], state, env, d);
  let result = Trampoline.run(~step_limit?, result);
  switch (result) {
  | Completed((_, _, x)) =>
    Completed((x |> Substitution.in_exp(env) |> Exp.replace_all_ids, state^))
  | StepLimitExceeded => StepLimitExceeded
  };
};

let evaluate =
    (
      ~targets: Sample.targets=Sample.no_targets,
      ~prev: IncrEval.t=IncrEval.empty,
      ~info_slice: IncrEval.InfoSlice.t=IncrEval.InfoSlice.empty,
      ~env,
      d: DHExp.t,
    )
    : (Exp.t, EvaluatorState.t) =>
  switch (evaluate_and_limit(~targets, ~prev, ~info_slice, ~env, d)) {
  | Completed(x) => x
  | StepLimitExceeded =>
    raise(Failure("Impossible: Step limit exceeded when not set"))
  };
