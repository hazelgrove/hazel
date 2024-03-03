/**
  State monad pattern.
 */

/**
  State type module specification.
 */
module type STATE = {
  type t;
};

/**
  Output of the functor [Make].
 */
module type S = {
  type state;

  include Monads.MONAD with type t('a) = state => (state, 'a);

  /**
    [get] fetches the current value of state within the monad.
   */
  let get: t(state);

  /**
    [put s] sets the state within the monad to [s].
   */
  let put: state => t(unit);

  /**
    [modify f] is an action that updates the state [s] to the result of [f s].
   */
  let modify: (state => state) => t(unit);

  /**
    [modify' f] is an action that updates the state [s] to the result of [snd (f
    s)] and returns the monad with the value [fst (f s)].
   */
  let modify': (state => ('a, state)) => t('a);
};

/**
  Functor to construct an implementation of the state monad for the given state
  type.
 */
module Make: (ST: STATE) => S with type state = ST.t;
