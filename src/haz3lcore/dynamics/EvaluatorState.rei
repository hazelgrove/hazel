/**
  This module is the state to be threaded throughout evaluation.

  Currently, it holds information about numbered environments and evaluation
  statistics (see {!module:EvaluatorStats}).

  This state may also be saved in {!type:EvaluatorResult.t} for resumed
  evaluation with the "fill-and-resume" functionality, when implemented.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t;

/**
  [init] is the initial state.
 */
let init: t;

/**
  [take_step es] is [es] with the updated step count.
 */
let take_step: t => t;

/**
  [get_step es] is the number of steps taken.
 */
let get_step: t => int;

let put_step: (int, t) => t;

let add_test: (t, Id.t, TestMap.instance_report) => t;

let get_tests: t => TestMap.t;

let put_tests: (TestMap.t, t) => t;
