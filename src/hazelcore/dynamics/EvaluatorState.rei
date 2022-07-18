/* EvaluatorState.re: Holds state to be threaded throughout evaluation.

   Currently holds information about numbered environments, and
   evaluation statistics (e.g., number of calls to `Evaluator.evaluate`).
   This state may be saved in the `EvaluationResult.t` for resumed
   evaluation with the "fill-and-resume" functionality, when implemented.
   */

[@deriving sexp]
type t = {
  ei: EnvironmentId.t,
  stats: EvaluatorStats.t,
};

let initial: t;

/* Generate the next unique environment identifier */
let next_env_id: t => (t, EnvironmentId.t);

let inc_eval_steps: t => t;

let eval_steps: t => int;
