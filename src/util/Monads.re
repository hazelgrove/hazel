/* TODO: we might want to just use this API?

    https://github.com/rgrinberg/ocaml-mtl/blob/master/lib/mtl.ml

   Though it's a bit heavy, especially with the extra type parameter...

   In any case, that's a good reference. */

module type MONAD_BASIC = {
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
};

module type MONAD_FUNCTOR = {
  include MONAD_BASIC;
  let map: (t('a), 'a => 'b) => t('b);
};

module Make_Functor = (M: MONAD_BASIC) => {
  include M;
  let map = (x, f) => bind(x, a => M.return(f(a)));
};

module type MONAD_ZIP = {
  include MONAD_FUNCTOR;
  let zip: (t('a), t('b)) => t(('a, 'b));
};

module Make_Zip = (M: MONAD_FUNCTOR) => {
  include M;

  let zip = (x, y) => bind(x, a => bind(y, b => M.return((a, b))));
};

module type MONAD = {
  include MONAD_ZIP;

  let sequence: list(t('a)) => t(list('a));

  module Syntax: {
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    let (let+): (t('a), 'a => 'b) => t('b);
    let (and+): (t('a), t('b)) => t(('a, 'b));

    let (>>=): (t('a), 'a => t('b)) => t('b);
    let (>>|): (t('a), 'a => 'b) => t('b);
  };
};

module Make_Monad_Z = (M: MONAD_ZIP) => {
  include M;

  let sequence = ms => {
    let rec sequence' = (ms, acc) => {
      switch (ms) {
      | [] => acc
      | [m, ...ms] =>
        bind(m, x => sequence'(ms, map(acc, acc => [x, ...acc])))
      };
    };

    map(sequence'(ms, [] |> return), List.rev);
  };

  module Syntax = {
    let ( let* ) = bind;
    let (let+) = map;
    let (and+) = zip;

    let (>>=) = bind;
    let (>>|) = map;
  };
};

module Make_Monad_B = (M: MONAD_BASIC) =>
  Make_Monad_Z(Make_Zip(Make_Functor(M)));
