open Sexplib.Std;

[@deriving sexp]
type t = {
  step: int,
  fuel: int,
};

/* TODO: Configurable fuel. */
let init = {step: 0, fuel: 32};

let take_step = ({step, _} as state) => {...state, step: step + 1};

let take_fuel = ({fuel, _} as state) => {...state, fuel: fuel + 1};

let out_of_fuel = ({fuel, _}) => fuel <= 0;
