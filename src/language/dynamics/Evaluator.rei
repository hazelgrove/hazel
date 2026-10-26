// INVARIANT: this evaluate function should never return an expression with closures.

type yielding_evaluation;

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

/* Incremental evaluator. Reuses entries from `prev` for non-deferred
 * sub-expressions whose elaboration (per `eval_info`) and co-ctx
 * dependencies are unchanged. Pass `prev=IncrEval.empty` and
 * `eval_info=EvalInfoMap.empty` (the defaults) to opt out of reuse.
 *
 * `eval_info` is a serializable projection of the statics info_map — the
 * full StaticsBase.Map.t embeds LivelitCtx closures and can't cross
 * postMessage. Callers with statics should project via
 * EvalInfoMap.of_info_map first.
 *
 * `statics` is the full statics map, only consulted by big-step proof
 * checking (which needs typing context that EvalInfoMap deliberately omits).
 * Defaults to empty; callers without statics or in postMessage paths should
 * leave it empty (proof-checking becomes a no-op there). */
/* Proof-level `eval` silent steps, shared by the checker's eval step and
 * the stepper's step picker: hidden (AutoStep) transitions and a LEADING
 * `VarLookup` (`is_proof_silent`), each collected as
 * (input, justification, output) with the resulting expression.
 * Unbounded unless `max_steps` is given (the checker passes none, and so
 * always gets Some); with it, None once `max_steps` steps were taken and
 * another remains. */
let is_proof_silent: EvaluatorStep.step => bool;

let advance_hidden:
  (~max_steps: int=?, ~env: Environment.t(Exp.t), Exp.t) =>
  option((list((Exp.t, string, Exp.t)), Exp.t));

let evaluate:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~statics: Statics.Map.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

let evaluate_and_limit:
  (
    ~step_limit: int,
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~statics: Statics.Map.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    ~outbox: ref(IncrEval.outbox(EvaluatorState.t))=?,
    Exp.t
  ) =>
  limited_result;

let start_yielding_evaluation:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    Exp.t
  ) =>
  yielding_evaluation;

let run_yielding_slice:
  (~step_budget: int, yielding_evaluation) => yielding_result;

let yielding_step_count: yielding_evaluation => int;

let drain_streaming_outbox:
  yielding_evaluation => IncrEval.outbox(EvaluatorState.t);
