[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  tests: TestMap.t,
  probes: Dynamics.Probe.Map.t,
};

let init = {tests: TestMap.empty, probes: Dynamics.Probe.Map.empty};

let add_test = ({tests, _} as es, id, report) => {
  let tests = tests |> TestMap.extend((id, report));
  {...es, tests};
};

let get_tests = ({tests, _}) => tests;

let add_closure = ({probes, _} as es, closure: Dynamics.Probe.Closure.t) => {
  ...es,
  probes: Dynamics.Probe.Map.extend(closure.syntax_id, closure, probes),
};

let get_probes = ({probes, _}) => probes;
