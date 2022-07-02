[@deriving sexp]
type t = (EvalEnvId.t, EvalStats.t);

let initial = (EvalEnvId.initial, EvalStats.initial);

let next_env_id = ((ei, stats): t): (t, EvalEnvId.t) => (
  (ei + 1, stats),
  ei,
);

let inc_eval_steps = ((ei, stats): t): t => (
  ei,
  stats |> EvalStats.inc_eval_steps,
);

let eval_steps = ((_, stats): t): int => stats |> EvalStats.eval_steps;
