let update_reuse_map_after_effects:
  (
    ~rhs_reused: Id.t => bool,
    ~reuse_map: IncrEval.reuse_map,
    list(EvaluatorState.effect)
  ) =>
  IncrEval.reuse_map;

let reuse_pass:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    Exp.t
  ) =>
  IncrEval.t(EvaluatorState.t);
