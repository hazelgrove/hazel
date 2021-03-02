/* TODO: we might want to just use this API?

    https://github.com/rgrinberg/ocaml-mtl/blob/master/lib/mtl.ml

   Though it's a bit heavy, especially with the extra type parameter...

   In any case, that's a good reference. */

module type MONAD_BASIC = {
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let map: [ | `Define_using_bind | `Custom((t('a), 'a => 'b) => t('b))];
};

module type MONAD = {
  include MONAD_BASIC;
  let map: (t('a), 'a => 'b) => t('b);
  let zip: (t('a), t('b)) => t(('a, 'b));
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
    | `Define_using_bind => bind(x, a => M.return(f(a)))
    | `Custom(mapper) => mapper(x, f)
    };

  let zip = (x, y) => bind(x, a => bind(y, b => M.return((a, b))));

  module Syntax = {
    let ( let* ) = bind;
    let (let+) = map;
    let (and+) = zip;
  };
};
