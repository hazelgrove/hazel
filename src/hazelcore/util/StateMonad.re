module type STATE = {type t;};

module type S = {
  type state;

  include Monads.MONAD with type t('a) = state => (state, 'a);

  let get: t(state);
  let put: state => t(unit);
  let update: (state => state) => t(unit);
};

module Make = (ST: STATE) => {
  type state = ST.t;

  module T = {
    [@deriving sexp]
    type t('a) = state => (state, 'a);

    let return = (x, s) => (s, x);

    let bind = (xf, f, s) => {
      let (s', x) = xf(s);
      f(x, s');
    };

    let get = s => (s, s);

    let put = (x, _) => (x, ());

    let update = f => bind(get, s => put(f(s)));
  };

  include T;
  include Monads.Make_Monad_B(T);
};
