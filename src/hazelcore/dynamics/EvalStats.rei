/* Store information about the current evaluation.Action

   Example information to stored:
   - Number of evaluation steps.
   - Number of times resumed.
   - Total evaluation time.
   */
[@deriving sexp]
type t;

let initial: t;

let inc_steps: t => t;

let get_steps: t => int;
