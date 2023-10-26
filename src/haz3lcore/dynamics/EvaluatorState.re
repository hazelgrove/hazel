[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  eig: EnvironmentIdGen.t,
  stats: EvaluatorStats.t,
  tests: TestMap.t,
  probes: ProbeMap.t,
};

let init = {
  eig: EnvironmentIdGen.init,
  stats: EvaluatorStats.initial,
  tests: TestMap.empty,
  probes: ProbeMap.empty,
};

let get_eig = ({eig, _}) => eig;
let put_eig = (eig, es) => {...es, eig};
let with_eig = (f, es) => {
  let (x, eig) = es |> get_eig |> f;
  (x, es |> put_eig(eig));
};

let take_step = ({stats, _} as es) => {
  ...es,
  stats: stats |> EvaluatorStats.take_step,
};

let get_step = ({stats, _}) => stats |> EvaluatorStats.get_step;

let add_test = ({tests, _} as es, id, report) => {
  let tests = tests |> TestMap.extend((id, report));
  {...es, tests};
};

let get_tests = ({tests, _}) => tests;

let put_tests = (tests, es) => {...es, tests};

let add_probe = ({probes, _} as es, id, instance) => {
  let probes = probes |> ProbeMap.extend(id, instance);
  {...es, probes};
};

let get_probes = ({probes, _}) => probes;

let put_probes = (probes, es) => {...es, probes};
