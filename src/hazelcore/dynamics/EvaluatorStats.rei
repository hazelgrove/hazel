/* EvaluatorStats.re: Holds statistics about the current program evaluation.

   Currently only holds the step count, but may be useful
   for holding additional information about the current evaluation
   (e.g., how many times it has been resumed w/ fill-and-resume).
   */
[@deriving sexp]
type t = int;

let initial: t;

let inc_eval_steps: t => t;

let eval_steps: t => int;
