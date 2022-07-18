/* EvaluatorState: Holds state to be threaded throughout evaluation.

   Currently holds information about numbered environments, and
   evaluation statistics (e.g., number of calls to `Evaluator.evaluate`).
   This state may be saved in the `EvaluationResult.t` for resumed
   evaluation with the "fill-and-resume" functionality, when implemented.
   */

[@deriving sexp]
type t;

let init: t;

let get_eig: t => EnvironmentIdGen.t;
let put_eig: (EnvironmentIdGen.t, t) => t;
let with_eig: (EnvironmentIdGen.t => ('a, EnvironmentIdGen.t), t) => ('a, t);

let take_step: t => t;
let get_step: t => int;

let add_test: (t, KeywordID.t, TestMap.instance_report) => t;
