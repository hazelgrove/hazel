// INVARIANT: this evaluate function should never return an expression with closures.

type yielding_evaluation;

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

let evaluate:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

/* evaluate, also returning the work done: the steps the trampoline
   actually took, a reused result counting as one. Unlike the state's
   step_count, it drops when results are reused. For Test_PerfBudgets. */
let evaluate_counting_work:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  ((Exp.t, EvaluatorState.t), int);

let evaluate_and_limit:
  (
    ~step_limit: int,
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
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
    ~reuse_stream: IncrEval.t(EvaluatorState.t)=?,
    Exp.t
  ) =>
  yielding_evaluation;

let run_yielding_slice:
  (~step_budget: int, yielding_evaluation) => yielding_result;

let yielding_step_count: yielding_evaluation => int;

let drain_streaming_outbox:
  yielding_evaluation => IncrEval.outbox(EvaluatorState.t);
