[@deriving sexp]
type t = {
  ei: EnvironmentId.t,
  stats: EvaluatorStats.t,
};

let initial = {ei: EnvironmentId.initial, stats: EvaluatorStats.initial};

let next_env_id = ({ei, _} as es: t): (t, EnvironmentId.t) => (
  {...es, ei: ei + 1},
  ei,
);

let inc_eval_steps = ({stats, _} as es: t): t => {
  ...es,
  stats: stats |> EvaluatorStats.inc_eval_steps,
};

let eval_steps = ({stats, _}: t): int => stats |> EvaluatorStats.eval_steps;
