// INVARIANT: this evaluate function should never return an expression with closures.

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

type yielding_evaluation;

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation)
  | EvaluationStepLimitExceeded;

let evaluate:
  (
    ~targets: Sample.targets=?,
    ~prev: IncrEval.t=?,
    ~info_map: EvalInfoMap.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

let evaluate_and_limit:
  (
    ~step_limit: int=?,
    ~targets: Sample.targets=?,
    ~prev: IncrEval.t=?,
    ~info_map: EvalInfoMap.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    Exp.t
  ) =>
  step_constrained((Exp.t, EvaluatorState.t));

let start_yielding_evaluation:
  (
    ~targets: Sample.targets=?,
    ~prev: IncrEval.t=?,
    ~info_map: EvalInfoMap.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    Exp.t
  ) =>
  yielding_evaluation;

let run_yielding_slice:
  (~step_limit: int=?, ~step_budget: int, yielding_evaluation) =>
  yielding_result;
