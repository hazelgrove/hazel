/* TODO: we might want to just use this API?

    https://github.com/rgrinberg/ocaml-mtl/blob/master/lib/mtl.ml

   Though it's a bit heavy, especially with the extra type parameter...

   In any case, that's a good reference. */

module MapDefinition = {
  type t('custom) =
    | Define_using_bind
    | Custom('custom);
};

module type MONAD_BASIC = {
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let map: MapDefinition.t((t('a), 'a => 'b) => t('b));
};

module type MONAD = {
  include MONAD_BASIC;
  let map: (t('a), 'a => 'b) => t('b);
  let zip: (t('a), t('b)) => t(('a, 'b));
  let sequence: list(t('a)) => t(list('a));
  module Syntax: {
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    let (let+): (t('a), 'a => 'b) => t('b);
    let (and+): (t('a), t('b)) => t(('a, 'b));
  };
};

module Make = (M: MONAD_BASIC) => {
  include M;

  let map = (x, f) =>
    switch (M.map) {
    | Define_using_bind => bind(x, a => M.return(f(a)))
    | Custom(mapper) => mapper(x, f)
    };

  let zip = (x, y) => bind(x, a => bind(y, b => M.return((a, b))));

  module Syntax = {
    let ( let* ) = bind;
    let (let+) = map;
    let (and+) = zip;
  };

  let sequence = (xs: list(t('a))): t(list('a)) =>
    List.fold_right(
      (x, result) => {
        open Syntax;
        let* x = x;
        let+ xs = result;
        [x, ...xs];
      },
      xs,
      return([]),
    );
};

/* State Monad */

module type STATE_MONAD_BASIC = {
  type state;
  include MONAD_BASIC with type t('result) = state => ('result, state);
};

module type STATE_MONAD = {
  include STATE_MONAD_BASIC;
  include MONAD with type t('result) = state => ('result, state);

  let run: (t('result), state) => ('result, state);
  let eval: (t('result), state) => 'result;
  let exec: (t('result), state) => state;

  let get: t(state);
  let put: state => t(unit);
};

module State = {
  module Make = (M: STATE_MONAD_BASIC) => {
    include M;
    include Make(M);

    let run = (m: t('result), s: state): ('result, state) => m(s);
    let eval = (m: t('result), s: state): 'result => s |> run(m) |> fst;
    let exec = (m: t('result), s: state): state => s |> run(m) |> snd;

    let get: t(state) = s => (s, s);
    let put = (s: state): t(unit) => _ => ((), s);
  };
};
