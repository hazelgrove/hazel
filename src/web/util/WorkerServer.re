open Util;

/* WorkerServer
 * ============
 *
 * Worker-side glue for the dynamic incremental evaluator. Owns:
 *   1. The wire types Editor <-> Worker share with WorkerClient (Request /
 *      Response, including `generation` echoed back as `processed_gen` so
 *      the client can detect lost updates around worker self-exit).
 *   2. The synchronous `work` entry point used by the main-thread fallback
 *      when queue_worker is None.
 *   3. The `Sched` submodule: a typed-state-machine cooperative scheduler
 *      that drives the resumable trampoline runner, polls a pending_update
 *      ref between budget chunks, and self-exits via self.close() once a
 *      batch finishes with no further updates queued.
 *
 * Layering: this file contains all worker-side mutability. The Evaluator
 * stays pure; cache filtering lives in IncrEval. */

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    targets: Language.Sample.targets,
    /* Projected statics data used by the incremental driver to look up
     * per-id sub-elaborations and co-ctxs. We ship this slice instead of
     * the full StaticsBase.Map.t because the full map transitively contains
     * LivelitCtx entries that embed OCaml closures, which the structured-
     * clone algorithm postMessage uses rejects. Pass the empty slice to
     * opt out of incremental reuse. */
    info_slice: Language.IncrEval.InfoSlice.t,
    /* Previous run's incremental map; pass IncrEval.empty on first run. */
    prev: Language.IncrEval.t,
    /* Ids the editor knows changed since the last successful evaluation
     * (computed from a fast Exp.fast_equal walk on the elab). Sent as a
     * list because Id.Set lacks ppx-derived serializers; the worker
     * converts to a set for fast lookup. Used as a fast invalidation hint
     * for the worker and to drive editor-side "still calculating"
     * overlays. */
    changed_ids: list(Id.t),
  };
  [@deriving (show, sexp, yojson)]
  type t = {
    /* Monotonically increasing across all editor sends. The worker echoes
     * the highest generation it observed in `Response.processed_gen`. */
    generation: int,
    cells: list((string, value)),
  };
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Language.Exp.t, Language.EvaluatorState.t),
      Language.ProgramResult.error,
    );

  /* Mid-flight snapshot of one cell's evaluator state. Carries the same
   * `EvaluatorState.t` the final Done payload would carry — probes,
   * tests, theorems, incr_eval — but without a finalized result Exp.t
   * (the cell hasn't reduced to a value yet). The editor mounts the
   * partial dynamics into its UI so probe samples populate as they're
   * captured rather than all-at-once on Done. */
  [@deriving (show, sexp, yojson)]
  type partial_value = Language.EvaluatorState.t;

  /* Two message kinds share one wire type so the worker's onmessage
   * stays a single dispatch point.
   *
   *   Progress: sent at each cooperative yield while a job is still
   *     running. The worker stays alive after a Progress; the editor
   *     should NOT spawn a fresh worker on Progress.
   *   Done:     sent once when all cells in the job have finished.
   *     The worker self-closes immediately after; the editor drops
   *     its worker_ref and lazily spawns a new one on the next
   *     request.
   *
   * `processed_gen` echoes the highest generation the worker has seen
   * at the moment of send, so the editor can drop stale messages
   * (e.g. a Progress whose generation is older than `last_sent_gen`,
   * meaning a newer request already eclipsed the run that produced
   * it). */
  [@deriving (show, sexp, yojson)]
  type t =
    | Progress({
        processed_gen: int,
        partials: list((string, partial_value)),
      })
    | Done({
        processed_gen: int,
        results: list((string, value)),
      });

  /* Convenience accessors used at message-dispatch sites. */
  let processed_gen =
    fun
    | Progress({processed_gen, _})
    | Done({processed_gen, _}) => processed_gen;

  let is_done =
    fun
    | Progress(_) => false
    | Done(_) => true;

  let (sexp_of_t, t_of_sexp) =
    Util.StructureShareSexp.structure_share_in(sexp_of_t, t_of_sexp);
};

/* Construct an Exp.t whose root id matches `at` and whose term is
 * Invalid("Timeout"). Surfaced at the original elab's root id so the
 * editor renders the timeout marker over the right token. The literal
 * `Invalid` constructor is type-disambiguated by the explicit Exp.term
 * annotation. */
let timeout_result_at = (at: Language.Exp.t): Language.Exp.t => {
  let id = Language.Exp.rep_id(at);
  let term: Language.Exp.term = Invalid("Timeout");
  Language.DHExp.mk([id], term);
};

/* Synchronous, run-to-completion work. Used by:
 *   - The main thread when `queue_worker` is None (small in-page evals).
 *   - The CLI evaluator.
 * Honors `step_limit`: returns Error(Timeout) on overrun. */
let work_sync =
    (~step_limit: option(int)=?, req_value: Request.value): Response.value => {
  let Request.{expr, targets, info_slice, prev, changed_ids: _} = req_value;
  switch (
    Language.Evaluator.evaluate_and_limit(
      ~step_limit?,
      ~targets,
      ~prev,
      ~info_slice,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception (Language.EvaluatorError.Exception(reason)) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exception exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  | Completed((result, state)) =>
    Ok((result, Language.EvaluatorState.clear_transient(state)))
  | StepLimitExceeded => Error(Language.ProgramResult.Timeout)
  };
};

/* Backward-compat name for callers that don't care about budgets. */
let work = (req_value: Request.value): Response.value =>
  work_sync(req_value);

module Sched = {
  /* Per-cell yield budget (trampoline steps per JS event-loop tick). Bigger
   * = less per-tick overhead, slower message-poll cadence. Tuned to keep
   * per-tick wall clock well under one frame. */
  let yield_step_budget = 200_000;

  /* Per-cell hard step cap (trampoline steps total before we declare
   * Invalid("Timeout") for this cell). Tuned to give clearly looping
   * programs a definite end without prematurely killing legitimate
   * long-running evals. */
  let cell_step_limit = 100_000_000;

  /* One cell's evaluation as a job: either currently running with a
   * suspended trampoline + state ref, or waiting to start. The result
   * accumulates so when all cells finish we can post Done. */
  type cell_status =
    | Pending
    | Running({
        susp:
          Language.Evaluator.Trampoline.suspended(Language.DHExp.t),
        state: ref(Language.EvaluatorState.t),
        env: Language.Environment.t(Language.Exp.t),
        steps_used: int,
      })
    | Finished(Response.value);

  type cell = {
    key,
    req: Request.value,
    status: cell_status,
  };

  type job = {
    cells: list(cell),
    generation: int,
  };

  type job_state =
    | Idle
    | Active(job);

  /* Minimum interval between Progress sends, in trampoline steps. The
   * scheduler ticks at `yield_step_budget` per tick (~one event-loop
   * yield), so this is effectively "every K ticks". Tuned to keep the
   * postMessage rate well under one frame's worth of work even for
   * tiny programs that finish each yield in a few hundred μs. The
   * editor's UI reflects samples within this window. */
  let progress_throttle_steps = yield_step_budget * 4;

  /* All worker-side mutability lives here. */
  type t = {
    mutable state: job_state,
    mutable pending_update: option(Request.t),
    mutable max_seen_gen: int,
    mutable scheduled: bool,
    /* Step count at which we last posted a Progress message. The next
     * Progress post is gated on `total_steps_used - last_progress_steps
     * >= progress_throttle_steps`, where `total_steps_used` aggregates
     * all cells' `steps_used`. Reset on restart so the first Progress
     * of a new run isn't delayed by the previous run's accounting. */
    mutable last_progress_steps: int,
  };

  let make = (): t => {
    state: Idle,
    pending_update: None,
    max_seen_gen: 0,
    scheduled: false,
    last_progress_steps: 0,
  };

  /* Build a fresh job from a Request.t: every cell starts in Pending. */
  let job_of_request = (req: Request.t): job => {
    cells:
      List.map(
        ((k, v)) =>
          {
            key: k,
            req: v,
            status: Pending,
          },
        req.cells,
      ),
    generation: req.generation,
  };

  /* Merge a new request into a running job: for each cell, if it's still
   * Pending or Running we carry over its partial cache (the running
   * incr_eval) and restart fresh against the new request. Already-Finished
   * cells from the prior request are dropped — the editor's fresh request
   * is the new ground truth.
   *
   * The partial cache is `filter_safe`'d against the new elaboration
   * (drops entries whose id is in `changed_ids` or that fail
   * `reuse_check`), then merged into the new request's `prev` so the
   * fresh evaluator can reuse both. */
  let restart_with = (sched: t, req: Request.t): unit => {
    let prior_partials: list((key, Language.IncrEval.t)) =
      switch (sched.state) {
      | Active(job) =>
        List.filter_map(
          c =>
            switch (c.status) {
            | Running({state, _}) => Some((c.key, state^.incr_eval))
            | _ => None
            },
          job.cells,
        )
      | Idle => []
      };
    let cells =
      List.map(
        ((k, v: Request.value)) => {
          let merged_prev =
            switch (List.assoc_opt(k, prior_partials)) {
            | Some(partial) =>
              let safe_partial =
                Language.IncrEval.filter_safe(
                  ~prev=partial,
                  ~changed_ids=v.changed_ids,
                  ~new_info_slice=v.info_slice,
                  ~new_targets=v.targets,
                  v.expr,
                );
              Language.IncrEval.merge(~newer=safe_partial, ~older=v.prev);
            | None => v.prev
            };
          {
            key: k,
            req: {
              ...v,
              prev: merged_prev,
            },
            status: Pending,
          };
        },
        req.cells,
      );
    sched.state =
      Active({
        cells,
        generation: req.generation,
      });
    sched.max_seen_gen = max(sched.max_seen_gen, req.generation);
    sched.last_progress_steps = 0;
  };

  /* Receive a request from the editor. Just stash it in pending_update;
   * the next `tick` will pick it up. This decoupling is what lets keys
   * not restart anything by themselves — the editor's outbox is on its
   * side, and our pending_update is the single point at which restart
   * decisions are made. */
  let rec on_message = (sched: t, req: Request.t): unit => {
    sched.pending_update = Some(req);
    sched.max_seen_gen = max(sched.max_seen_gen, req.generation);
    if (!sched.scheduled) {
      sched.scheduled = true;
      ignore(
        Js_of_ocaml.Dom_html.window##setTimeout(
          Js_of_ocaml.Js.wrap_callback(() => tick(sched)),
          0.0,
        ),
      );
    };
  }
  /* Run one cooperative tick: drain pending_update if any, then advance
   * one cell by a budget chunk. Either schedules another tick (more work)
   * or posts Done + self-closes (all cells finished). */
  and tick = (sched: t): unit => {
    sched.scheduled = false;
    /* Drain pending_update first: if a fresh request arrived between
     * ticks, it always wins over what we were doing before. */
    switch (sched.pending_update) {
    | Some(req) =>
      sched.pending_update = None;
      restart_with(sched, req);
    | None => ()
    };
    switch (sched.state) {
    | Idle => ()
    | Active(job) =>
      let advanced = advance_one_cell(job);
      sched.state = Active(advanced);
      if (all_finished(advanced)) {
        post_done_and_close(sched, advanced);
      } else {
        /* Progress send is throttled by `progress_throttle_steps`. Even
         * with the throttle, the worker may post several Progress
         * messages per second on a long-running cell, each carrying a
         * fresh `EvaluatorState.t` snapshot. */
        maybe_post_progress(sched, advanced);
        schedule_tick(sched);
      };
    };
  }
  and advance_one_cell = (job: job): job => {
    /* Find the first cell that's not finished and run a chunk of it. */
    let rec go = (acc, remaining) =>
      switch (remaining) {
      | [] => List.rev(acc)
      | [cell, ...rest] =>
        switch (cell.status) {
        | Finished(_) => go([cell, ...acc], rest)
        | Pending =>
          let cell' = start_cell(cell);
          List.rev_append([cell', ...acc], rest);
        | Running(_) =>
          let cell' = step_cell(cell);
          List.rev_append([cell', ...acc], rest);
        }
      };
    {
      ...job,
      cells: go([], job.cells),
    };
  }
  and start_cell = (cell: cell): cell => {
    let Request.{expr, targets, prev, info_slice, changed_ids: _} = cell.req;
    let env = Language.Builtins.env_init;
    let (state, trampoline) =
      Language.Evaluator.evaluate_trampoline(
        ~targets,
        ~prev,
        ~info_slice,
        ~env,
        expr,
      );
    let init_chunk =
      Language.Evaluator.Trampoline.run_chunk(
        ~step_limit=cell_step_limit,
        ~step_budget=yield_step_budget,
        trampoline,
        Language.Evaluator.Trampoline.finished,
      );
    finish_chunk(cell, ~state, ~env, ~steps_used=yield_step_budget, init_chunk);
  }
  and step_cell = (cell: cell): cell =>
    switch (cell.status) {
    | Running({susp, state, env, steps_used}) =>
      let remaining_budget = cell_step_limit - steps_used;
      if (remaining_budget <= 0) {
        let timeout = timeout_result_at(cell.req.expr);
        {
          ...cell,
          status:
            Finished(
              Ok((timeout, Language.EvaluatorState.clear_transient(state^))),
            ),
        };
      } else {
        let budget = min(yield_step_budget, remaining_budget);
        let chunk =
          Language.Evaluator.Trampoline.resume_chunk(
            ~step_budget=budget,
            ~step_limit=remaining_budget,
            susp,
          );
        finish_chunk(
          cell,
          ~state,
          ~env,
          ~steps_used=steps_used + budget,
          chunk,
        );
      };
    | Pending
    | Finished(_) => cell
    }
  and finish_chunk =
      (
        cell: cell,
        ~state: ref(Language.EvaluatorState.t),
        ~env: Language.Environment.t(Language.Exp.t),
        ~steps_used: int,
        chunk: Language.Evaluator.Trampoline.chunk_result(Language.DHExp.t),
      )
      : cell =>
    switch (chunk) {
    | Completed(dh_value) =>
      let result = Language.Evaluator.finalize_value(~env, dh_value);
      {
        ...cell,
        status:
          Finished(
            Ok((result, Language.EvaluatorState.clear_transient(state^))),
          ),
      };
    | Suspended(susp) => {
        ...cell,
        status: Running({susp, state, env, steps_used}),
      }
    | StepLimitExceeded =>
      let timeout = timeout_result_at(cell.req.expr);
      {
        ...cell,
        status:
          Finished(
            Ok((timeout, Language.EvaluatorState.clear_transient(state^))),
          ),
      };
    | exception (Language.EvaluatorError.Exception(reason)) =>
      print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
      {
        ...cell,
        status: Finished(Error(Language.ProgramResult.EvaulatorError(reason))),
      };
    | exception exn =>
      print_endline("EXN:" ++ Printexc.to_string(exn));
      {
        ...cell,
        status:
          Finished(
            Error(
              Language.ProgramResult.UnknownException(Printexc.to_string(exn)),
            ),
          ),
      };
    }
  and all_finished = (job: job): bool =>
    List.for_all(
      c =>
        switch (c.status) {
        | Finished(_) => true
        | _ => false
        },
      job.cells,
    )
  /* Aggregate all cells' steps_used, so the throttle is across the
   * whole batch and not per-cell (a multi-cell exercise wouldn't want
   * each cell flooding its own Progress stream). */
  and total_steps = (job: job): int =>
    List.fold_left(
      (acc, c) =>
        switch (c.status) {
        | Running({steps_used, _}) => acc + steps_used
        | _ => acc
        },
      0,
      job.cells,
    )
  /* Snapshot every cell that's mid-evaluation. `Running` cells supply
   * the current state ref; `Pending` cells haven't started yet (skip);
   * `Finished` cells are reported with their full state so the editor
   * can mount probe samples for cells that completed early in the
   * batch even though others are still running. */
  and snapshot_partials =
      (job: job): list((string, Response.partial_value)) =>
    List.filter_map(
      c =>
        switch (c.status) {
        | Running({state, _}) =>
          Some((
            c.key,
            Language.EvaluatorState.clear_transient(state^),
          ))
        | Finished(Ok((_, state))) => Some((c.key, state))
        | Finished(Error(_))
        | Pending => None
        },
      job.cells,
    )
  and maybe_post_progress = (sched: t, job: job): unit => {
    let steps = total_steps(job);
    if (steps - sched.last_progress_steps >= progress_throttle_steps) {
      sched.last_progress_steps = steps;
      let resp: Response.t =
        Progress({
          processed_gen: max(sched.max_seen_gen, job.generation),
          partials: snapshot_partials(job),
        });
      Js_of_ocaml.Worker.post_message(resp);
    };
  }
  and post_done_and_close = (sched: t, job: job): unit => {
    let results =
      List.map(
        c =>
          (
            c.key,
            switch (c.status) {
            | Finished(v) => v
            | _ =>
              Error(Language.ProgramResult.UnknownException("Impossible"))
            },
          ),
        job.cells,
      );
    let resp: Response.t =
      Done({
        processed_gen: max(sched.max_seen_gen, job.generation),
        results,
      });
    sched.state = Idle;
    Js_of_ocaml.Worker.post_message(resp);
    /* Replace onmessage with a no-op so any racing edits posted between
     * our `post_message` above and `self.close()` below silently land in
     * a worker that's about to exit (the editor will detect the
     * lost-update via processed_gen and respawn). */
    Js_of_ocaml.Worker.set_onmessage(_ => ());
    /* Self-close to free the worker process. `self.close()` is the
     * standard webworker termination call; we reach it through
     * `Js.Unsafe.global##.close` (a method call, distinct from
     * `##close` which is a property read). */
    Js_of_ocaml.Js.Unsafe.fun_call(
      Js_of_ocaml.Js.Unsafe.global##.close,
      [||],
    );
  }
  and schedule_tick = (sched: t): unit =>
    if (!sched.scheduled) {
      sched.scheduled = true;
      ignore(
        Js_of_ocaml.Dom_html.window##setTimeout(
          Js_of_ocaml.Js.wrap_callback(() => tick(sched)),
          0.0,
        ),
      );
    };
};

let on_request = (sched: Sched.t, req: Request.t): unit =>
  Sched.on_message(sched, req);

let start = () => {
  let sched = Sched.make();
  Js_of_ocaml.Worker.set_onmessage(on_request(sched));
};
