open Sexplib.Std;

[@deriving sexp]
type t = {
  step: int,
  fuel: int,
};

/* TODO: Configurable fuel. */
let init = {step: 0, fuel: 256};

let take_step = ({step, _} as state) => {...state, step: step + 1};

let take_fuel = ({fuel, _} as state, x: int) => {...state, fuel: fuel - x};

let out_of_fuel = ({fuel, _}) => fuel <= 0;
