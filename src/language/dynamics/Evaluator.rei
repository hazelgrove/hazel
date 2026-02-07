// INVARIANT: this evaluate function should never return an expression with closures.

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

let evaluate:
  (~targets: Sample.targets=?, ~env: Environment.t(Exp.t), Exp.t) =>
  (Exp.t, EvaluatorState.t);

let evaluate_and_limit:
  (
    ~step_limit: int=?,
    ~targets: Sample.targets=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  step_constrained((Exp.t, EvaluatorState.t));
