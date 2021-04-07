/* Inspired by Jane Street's Monad:
 * https://ocaml.janestreet.com/ocaml-core/v0.13/doc/base/Base__/Monad_intf/index.html */

module type MONAD_BASIC = {
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

module Functor_of_Basic:
  (M: MONAD_BASIC) => MONAD_FUNCTOR with type t('a) = M.t('a);
module Zip_of_Functor:
  (M: MONAD_FUNCTOR) => MONAD_ZIP with type t('a) = M.t('a);

module type MONAD = {
  include MONAD_ZIP;
  module Syntax: {
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    let (let+): (t('a), 'a => 'b) => t('b);
    let (and+): (t('a), t('b)) => t(('a, 'b));
  };
};

// See ActionOutcome.re{i} for an example
module MakeB: (M: MONAD_BASIC) => MONAD with type t('a) = M.t('a);
module MakeZ: (M: MONAD_ZIP) => MONAD with type t('a) = M.t('a);
