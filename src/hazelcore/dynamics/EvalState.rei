/* Evaluation state. Used to store information that is threaded
   throughout calls to `Evaluator.evaluate`, such as the environment
   id generator (so that all environment ID's are unique), evaluation
   statistics, and fill-and-resume information.

   All of these functions return the update `EvalState.t` as the first
   parameter.
   */
[@deriving sexp]
type t;

/* Constructor used when beginning evaluation */
let initial: t;

/* Emits a new and unique `EvalEnvId.t`. */
let next_evalenvid: t => (t, EvalEnvId.t);

/* Set the fill-and-resume information for the current evaluation. */
let set_far_info: (FARInfo.t, t) => t;

/* Gets the expression to fill if the hole number matches the hole number
   in the FAR info. */
let get_fill_dhexp: (MetaVar.t, t) => option(DHExp.t);

/* Getter for statistics */
let get_stats: t => EvalStats.t;

/* Update number of evaluation steps in statistics. */
let inc_steps: t => t;
