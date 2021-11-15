open Sexplib.Std;

[@deriving sexp]
type t = {
  test_map: TestMap.t,
  step: int,
  fuel: int,
};

let init_fuel = 256;

let init = {test_map: TestMap.empty, step: 0, fuel: init_fuel};

let take_step = ({step, _} as state: t): t => {...state, step: step + 1};

let burn_fuel = ({fuel, _} as state: t): t => {...state, fuel: fuel - 1};

let add_test = ({test_map, _} as state: t, n, result): t => {
  ...state,
  test_map: TestMap.extend((n, result), test_map),
};
