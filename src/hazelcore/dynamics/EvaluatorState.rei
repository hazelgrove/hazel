/**
 * The type for the state accumulated during evaluation.
 */
[@deriving sexp]
type t = {
  step: int,
  gas: int,
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
 * {take_gas state} decrements the amount of gas in {state}.
 */
let take_gas: (t, int) => t;

/**
 * {out_of_gas t} returns {true} if there is no more gas.
 */
let out_of_gas: t => bool;
