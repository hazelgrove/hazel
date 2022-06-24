/* Inspired by Jane Street's Monad:
 * https://ocaml.janestreet.com/ocaml-core/v0.13/doc/base/Base__/Monad_intf/index.html */

module MapDefinition: {
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

// Use `includ Monads.Make` with a MONAD_BASIC to get a MONAD
// See ActionOutcome.re{i} for an example
module type MONAD = {
  include MONAD_BASIC;
  let map: (t('a), 'a => 'b) => t('b);
  let zip: (t('a), t('b)) => t(('a, 'b));
  module Syntax: {
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    let (let+): (t('a), 'a => 'b) => t('b);
    let (and+): (t('a), t('b)) => t(('a, 'b));

    let (>>=): (t('a), 'a => t('b)) => t('b);
    let (>>|): (t('a), 'a => 'b) => t('b);
  };
};

module Make: (M: MONAD_BASIC) => MONAD with type t('a) := M.t('a);
