/**
 * The type for the state accumulated during evaluation.
 */
[@deriving sexp]
type t = {
  step: int,
  fuel: int,
};

/**
 * {init} is the initial state.
 */
let init: t;

/**
 * {take_step state} increments the step count in {state}.
 */
let take_step: t => t;

/**
 * {take_fuel state} decrements the amount of fuel in {state}.
 */
let take_fuel: t => t;

/**
 * {out_of_fuel t} returns {true} if there is no more fuel.
 */
let out_of_fuel: t => bool;
