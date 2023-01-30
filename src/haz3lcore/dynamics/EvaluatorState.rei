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
  [get_eig es] is current environment id generator.
 */
let get_eig: t => EnvironmentIdGen.t;

/**
  [put_eig eig es] is [es] with the environment id generator [eig].
 */
let put_eig: (EnvironmentIdGen.t, t) => t;

/**
  [with_eig f es] calls [f] with the current environment id generator, updating
  [es] afterwards.
 */
let with_eig: (EnvironmentIdGen.t => ('a, EnvironmentIdGen.t), t) => ('a, t);

/**
  [take_step es] is [es] with the updated step count.
 */
let take_step: t => t;

/**
  [get_step es] is the number of steps taken.
 */
let get_step: t => int;

let add_test: (t, Id.t, TestMap.instance_report) => t;
let get_tests: t => TestMap.t;
let put_tests: (TestMap.t, t) => t;

let add_probe: (t, Id.t, ProbeMap.instance) => t;
let get_probes: t => ProbeMap.t;
let put_probes: (ProbeMap.t, t) => t;
