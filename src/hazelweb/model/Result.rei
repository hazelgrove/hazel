/* The result of a program evaluation.

   Components:
   - `EvaluatorResult.t`: (postprocessed) evaluation result
   - `HoleClosureInfo.t`: information about the holes/hole
      closure numbers (from postprocessing)
   - `DHExp.t`: un-postprocessed evaluation result (start point for
     evaluation of fill-and-resume)
   - `Delta.t`: hole context info (from elaboration, for fill-and-resume)
   - `EvalState.t`: evaluation state (for fill-and-resume)
   */
[@deriving sexp]
type t;

/* First argument is the postprocessed result, second argument is
   the un-postprocessed result (raw evaluation result). */
let mk:
  (EvaluatorResult.t, DHExp.t, HoleClosureInfo.t, Delta.t, EvalState.t) => t;

/* For displaying the result */
let get_dhexp: t => DHExp.t;
let get_result: t => EvaluatorResult.t;
let get_hci: t => HoleClosureInfo.t;

/* For fill-and-resume/continuing evaluation */
let get_delta: t => Delta.t;
let get_eval_state: t => EvalState.t;
let get_unpostprocessed_dhexp: t => DHExp.t;

/* See DHExp.fast_equals. Also checks that all environments
   in the HoleClosureInfo.t are equal. */
let fast_equals: (t, t) => bool;
