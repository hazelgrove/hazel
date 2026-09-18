let update_reuse_map_after_effects:
  (
    ~tuple_flags: bool,
    ~reused: Id.t => bool,
    ~flags_from: IncrEval.reuse_map=?,
    ~reuse_map: IncrEval.reuse_map,
    list(EvaluatorState.effect)
  ) =>
  IncrEval.reuse_map;

let reuse_pass:
  (
    ~tuple_flags: bool=?,
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    Exp.t
  ) =>
  IncrEval.t(EvaluatorState.t);
