// INVARIANT: this evaluate function should never return an expression with closures.

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

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
