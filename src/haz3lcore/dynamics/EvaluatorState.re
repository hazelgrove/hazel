[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  eig: EnvironmentIdGen.t,
  stats: EvaluatorStats.t,
  tests: TestMap.t,
};

let init = {
  id: 0,
  eig: EnvironmentIdGen.init,
  stats: EvaluatorStats.initial,
  tests: TestMap.empty,
};

let get_eig = ({eig, _}) => eig;
let put_eig = (eig, es) => {...es, eig};
let with_eig = (f, es) => {
  let (x, eig) = es |> get_eig |> f;
  (x, es |> put_eig(eig));
};

let get_id = ({id, _}) => id;
let put_id = (id, es) => {...es, id};
let with_id = (f, es) => {
  let (x, id) = es |> get_id |> f;
  (x, es |> put_id(id));
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
