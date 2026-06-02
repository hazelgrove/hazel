// INVARIANT: this evaluate function should never return an expression with closures.

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

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
