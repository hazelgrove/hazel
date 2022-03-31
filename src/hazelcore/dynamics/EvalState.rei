/* Evaluation state. Used to store information that is threaded
   throughout calls to `Evaluator.evaluate`, such as the environment
   id generator (so that all environment ID's are unique) and
   evaluation statistics.

   All of these functions return the update `EvalState.t` as the first
   parameter.
   */
[@deriving sexp]
type t;

/* Constructor used when beginning evaluation */
let initial: t;

/* Emits a new and unique `EvalEnvId.t`. */
let next_evalenvid: t => (t, EvalEnvId.t);

/* Getter for statistics */
let get_stats: t => EvalStats.t;

/* Update number of evaluation steps in statistics. */
let inc_steps: t => t;
