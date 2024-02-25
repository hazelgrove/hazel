[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  stats: EvaluatorStats.t,
  tests: TestMap.t,
};

let init = {stats: EvaluatorStats.initial, tests: TestMap.empty};

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

let put_tests = (tests, es) => {...es, tests};
