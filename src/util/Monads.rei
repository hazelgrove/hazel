/* Inspired by Jane Street's Monad:
 * https://ocaml.janestreet.com/ocaml-core/v0.13/doc/base/Base__/Monad_intf/index.html */

module type MONAD_BASIC = {
  [@deriving sexp]
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
};

module type MONAD_FUNCTOR = {
  include MONAD_BASIC;
  let map: (t('a), 'a => 'b) => t('b);
};

module type MONAD_ZIP = {
  include MONAD_FUNCTOR;
  let zip: (t('a), t('b)) => t(('a, 'b));
};

module Make_Functor:
  (M: MONAD_BASIC) => MONAD_FUNCTOR with type t('a) = M.t('a);
module Make_Zip: (M: MONAD_FUNCTOR) => MONAD_ZIP with type t('a) = M.t('a);

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

// See ActionOutcome.re{i} for an example
module Make_Monad_B: (M: MONAD_BASIC) => MONAD with type t('a) = M.t('a);
module Make_Monad_Z: (M: MONAD_ZIP) => MONAD with type t('a) = M.t('a);
