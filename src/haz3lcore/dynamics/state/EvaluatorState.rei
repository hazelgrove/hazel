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
