[@deriving sexp]
type t = (EvalEnvId.t, EvaluatorStats.t);

let initial = (EvalEnvId.initial, EvaluatorStats.initial);

let next_env_id = ((ei, stats): t): (t, EvalEnvId.t) => (
  (ei + 1, stats),
  ei,
);

let inc_eval_steps = ((ei, stats): t): t => (
  ei,
  stats |> EvaluatorStats.inc_eval_steps,
);

let eval_steps = ((_, stats): t): int => stats |> EvaluatorStats.eval_steps;
