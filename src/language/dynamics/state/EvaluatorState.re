open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  theorems: list((Id.t, TermBase.environment_t, Typ.t)),
  tests: TestMap.t,
  probes: Dynamics.Probe.Map.t,
};

let init = {
  tests: TestMap.empty,
  probes: Dynamics.Probe.Map.empty,
  theorems: [],
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

let add_theorem = ({theorems, _} as es, id, env, goal) => {
  {
    ...es,
    theorems: theorems |> List.append([(id, env, goal)]),
  };
};

let get_theorems = ({theorems, _}) => theorems;
