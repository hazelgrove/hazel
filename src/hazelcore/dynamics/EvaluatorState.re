open Sexplib.Std;

[@deriving sexp]
type t = {
  step: int,
  gas: int,
};

/* TODO: Configurable gas. */
let init = {step: 0, gas: 256};

let take_step = ({step, _} as state) => {...state, step: step + 1};

let take_gas = ({gas, _} as state, x: int) => {...state, gas: gas - x};

let out_of_gas = ({gas, _}) => gas <= 0;
