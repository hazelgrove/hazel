let reuse_pass:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~info_map: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    Exp.t
  ) =>
  IncrEval.t(EvaluatorState.t);
