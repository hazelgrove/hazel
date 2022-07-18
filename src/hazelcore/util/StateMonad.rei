module type STATE = {type t;};

module type S = {
  type state;

  include Monads.MONAD with type t('a) = state => (state, 'a);

  let get: t(state);
  let put: state => t(unit);
};

module Make: (ST: STATE) => S with type state = ST.t;
