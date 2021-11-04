open Sexplib.Std;

[@deriving sexp]
type t = {
  assert_map: AssertMap.t,
  step: int,
  fuel: int,
};

let init_fuel = 256;

let init = {assert_map: AssertMap.empty, step: 0, fuel: init_fuel};

let take_step = ({step, _} as state: t): t => {...state, step: step + 1};

let burn_fuel = ({fuel, _} as state: t): t => {...state, fuel: fuel - 1};

let add_assert = ({assert_map, _} as state: t, n, result): t => {
  ...state,
  assert_map: AssertMap.extend((n, result), assert_map),
};
