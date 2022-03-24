open Sexplib.Std;

/* Current implementation: store number of evaluation steps. */
[@deriving sexp]
type t = int;

let initial = 0;

let inc_steps = (steps: t): t => steps + 1;

let get_steps = (steps: t): int => steps;
