[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  stats: EvaluatorStats.t,
  tests: TestMap.t,
  probes: Dynamics.Probe.Map.t,
};

let init = {
  stats: EvaluatorStats.initial,
  tests: TestMap.empty,
  probes: Dynamics.Probe.Map.empty,
};

let take_step = ({stats, _} as es) => {
  ...es,
  stats: stats |> EvaluatorStats.take_step,
};

let get_step = ({stats, _}) => stats |> EvaluatorStats.get_step;

let put_step = (step, es) => {...es, stats: EvaluatorStats.put_step(step)};

let add_test = ({tests, _} as es, id, report) => {
  let tests = tests |> TestMap.extend((id, report));
  {...es, tests};
};

let get_tests = ({tests, _}) => tests;

let add_probe = ({probes, _} as es, id: Id.t, v: Dynamics.Probe.Info.t) => {
  let probes = Dynamics.Probe.Map.extend(id, v, probes);
  {...es, probes};
};

let get_probes = ({probes, _}) => probes;
