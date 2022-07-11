[@deriving sexp]
type t = (EvalEnvId.t, EvalStats.t, TestMap.t);

let initial = (EvalEnvId.initial, EvalStats.initial, TestMap.empty);

let next_env_id = ((ei, stats, test_map): t): (t, EvalEnvId.t) => (
  (ei + 1, stats, test_map),
  ei,
);

let inc_eval_steps = ((ei, stats, test_map): t): t => (
  ei,
  stats |> EvalStats.inc_eval_steps,
  test_map,
);

let eval_steps = ((_, stats, _): t): int => stats |> EvalStats.eval_steps;

let add_test = ((ei, stats, test_map): t, n, report): t => {
  let test_map = test_map |> TestMap.extend((n, report));
  (ei, stats, test_map);
};
