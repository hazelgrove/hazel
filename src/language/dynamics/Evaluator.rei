// INVARIANT: this evaluate function should never return an expression with closures.

let evaluate:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

/* Doesn't include steps in re-used evaluation. Used in perf testing. */
let evaluate_counting_trampoline_steps:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  ((Exp.t, EvaluatorState.t), int);

module Limited: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type result =
    | LimitedCompleted((Exp.t, EvaluatorState.t))
    | StepLimitExceeded;

  let evaluate:
    (
      ~step_limit: int,
      ~prev: EvaluatorState.incr_eval=?,
      ~eval_info: EvalInfo.t=?,
      ~env: Environment.t(Exp.t),
      ~reuse_map: IncrEval.reuse_map=?,
      ~outbox: ref(IncrEval.outbox(EvaluatorState.t))=?,
      Exp.t
    ) =>
    result;
};

module Yielding: {
  type t;

  let step_count: t => int;

  let drain_outbox: t => IncrEval.outbox(EvaluatorState.t);

  type result =
    | EvaluationCompleted((Exp.t, EvaluatorState.t))
    | EvaluationYielded(t);

  let start:
    (
      ~prev: EvaluatorState.incr_eval=?,
      ~eval_info: EvalInfo.t=?,
      ~env: Environment.t(Exp.t),
      ~reuse_map: IncrEval.reuse_map=?,
      ~reuse_stream: IncrEval.t(EvaluatorState.t)=?,
      Exp.t
    ) =>
    t;

  let run_slice: (~step_budget: int, t) => result;
};
