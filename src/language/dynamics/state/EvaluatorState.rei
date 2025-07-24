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

let add_test: (t, Id.t, TestMap.instance_report) => t;

let get_tests: t => TestMap.t;

let add_closure: (t, Dynamics.Probe.Closure.t) => t;

let get_probes: t => Dynamics.Probe.Map.t;

let add_theorem: (t, Id.t, Environment.t, Typ.t) => t;

let get_theorems: t => list((Id.t, Environment.t, Typ.t));
