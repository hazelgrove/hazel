// INVARIANT: this evaluate function should never return an expression with closures.

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

/* Resumable trampoline runner. The worker scheduler (WorkerServer.Sched)
 * drives evaluation via `run_chunk` / `resume_chunk`, yielding to the JS
 * event loop on a step budget so editor postMessages can interleave.
 * Non-cooperative callers use `evaluate` / `evaluate_and_limit` below,
 * which collapse Suspended back into more chunks internally. */
module Trampoline: {
  type t('a);
  type callstack('a, 'b);
  type suspended('a);

  type chunk_result('a) =
    | Completed('a)
    | Suspended(suspended('a))
    | StepLimitExceeded;

  /* Start a fresh trampoline at a `Finished` callstack.
   *
   * `min_deadline` is the smallest active per-id timeout deadline (used
   * by the runner to short-circuit the per-step abort check); start a
   * fresh trampoline with the default `max_int` (no live deadlines). */
  let run_chunk:
    (
      ~step_limit: int=?,
      ~step_budget: int,
      ~step_counter: int=?,
      ~min_deadline: int=?,
      t('b),
      callstack('b, 'a)
    ) =>
    chunk_result('a);

  /* Resume a previously-suspended computation. */
  let resume_chunk:
    (~step_limit: int=?, ~step_budget: int, suspended('a)) => chunk_result('a);

  /* Convenience: drive to completion in a single sync call (used by CLI
   * and main-thread eval). */
  let run: (~step_limit: int=?, t('a)) => step_constrained('a);

  /* Top-level callstack value, paired with `run_chunk`. */
  let finished: callstack('a, 'a);

  /* Trampoline term builders. Constructors are not exposed directly;
   * callers use these helpers (or the `let.trampoline` ppx in `Syntax`)
   * to compose computations. */
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);

  /* Run `body` under an absolute budget of `budget` trampoline steps;
   * on overrun, the body is aborted mid-flight and `fallback` is
   * propagated as the body's value to the surrounding continuation.
   * Side effects performed before the abort persist. */
  let with_budget: (~budget: int, ~fallback: 'a, t('a)) => t('a);
};

/* Incremental evaluator. Reuses entries from `prev` for non-deferred
 * sub-expressions whose elaboration (per `info_slice`) and co-ctx
 * dependencies are unchanged. Pass `prev=IncrEval.empty` and
 * `info_slice=IncrEval.InfoSlice.empty` (the defaults) to opt out of reuse.
 *
 * `info_slice` is a serializable projection of the statics info_map — the
 * full StaticsBase.Map.t embeds LivelitCtx closures and can't cross
 * postMessage. Callers with an info_map should project via
 * IncrEval.InfoSlice.of_info_map first. */
let evaluate:
  (
    ~targets: Sample.targets=?,
    ~prev: IncrEval.t=?,
    ~info_slice: IncrEval.InfoSlice.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

let evaluate_and_limit:
  (
    ~step_limit: int=?,
    ~targets: Sample.targets=?,
    ~prev: IncrEval.t=?,
    ~info_slice: IncrEval.InfoSlice.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  step_constrained((Exp.t, EvaluatorState.t));

/* Build a trampoline for the cooperative scheduler. The state ref is
 * exposed so the scheduler can snapshot `state^.incr_eval` between
 * chunks for partial-cache merging on restart. */
let evaluate_trampoline:
  (
    ~targets: Sample.targets=?,
    ~prev: IncrEval.t=?,
    ~info_slice: IncrEval.InfoSlice.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (ref(EvaluatorState.t), Trampoline.t(DHExp.t));

/* Substitute env into a DHExp value and assign fresh ids. Run on the
 * DHExp returned by a `Completed` trampoline before sending it back. */
let finalize_value: (~env: Environment.t(Exp.t), DHExp.t) => Exp.t;
