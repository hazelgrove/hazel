open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  tests: TestMap.t,
  probes: Dynamics.Probe.Map.t,
  trace_length: int, // Trace length
  instantiations: int // Count # of instantiations
};

let init = {
  tests: TestMap.empty,
  probes: Dynamics.Probe.Map.empty,
  trace_length: 0,
  instantiations: 0,
};

let add_test = ({tests, _} as es, id, report) => {
  let tests = tests |> TestMap.extend((id, report));
  {
    ...es,
    tests,
  };
};

let get_tests = ({tests, _}) => tests;

let add_closure = ({probes, _} as es, closure: Dynamics.Probe.Closure.t) => {
  ...es,
  probes: Dynamics.Probe.Map.extend(closure.syntax_id, closure, probes),
};

let get_probes = ({probes, _}) => probes;

let get_trace_length = ({trace_length, _}) => trace_length;
let get_instantiations = ({instantiations, _}) => instantiations;

let incr_instantiations = (n, s) => {
  ...s,
  instantiations: s.instantiations + n,
};
let incr_trace = (n, s) => {
  ...s,
  trace_length: s.trace_length + n,
};
