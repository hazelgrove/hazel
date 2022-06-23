/* EvalState.re: Holds state to be threaded throughout evaluation.

   Currently holds information about numbered environments, and
   evaluation statistics (e.g., number of calls to `Evaluator.evaluate`).
   This state may be saved in the `EvaluationResult.t` for resumed
   evaluation with the "fill-and-resume" functionality, when implemented.
   */

[@deriving sexp]
type t = (EvalEnvId.t, EvalStats.t);

let initial = (EvalEnvId.initial, EvalStats.initial);

/* Generate the next unique environment identifier */
let next_env_id = ((ei, stats): t): (t, EvalEnvId.t) => (
  (ei + 1, stats),
  ei,
);

let inc_eval_steps = ((ei, stats): t): t => (
  ei,
  stats |> EvalStats.inc_eval_steps,
);

let eval_steps = ((_, stats): t): int => stats |> EvalStats.eval_steps;
