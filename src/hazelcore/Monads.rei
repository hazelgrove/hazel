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

// Use `include Monads.Make` with a MONAD_BASIC to get a MONAD
// See ActionOutcome.re{i} for an example
module type MONAD = {
  include MONAD_BASIC;
  let map: (t('a), 'a => 'b) => t('b);
  let zip: (t('a), t('b)) => t(('a, 'b));
  let sequence: list(t('a)) => t(list('a));
  let skip: (t('a), t('b)) => t('b);
  let liftM: ('a => 'b, t('a)) => t('b);
  let ap: (t('a => 'b), t('a)) => t('b);
  module Syntax: {
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    let (let+): (t('a), 'a => 'b) => t('b);
    let (and+): (t('a), t('b)) => t(('a, 'b));
  };

  // a terse notation for defining monadic operations
  //
  // $  sequence
  // @  bind
  // ^  apply (low, right)
  // +  apply (mid, left)
  // %  compose
  // ** apply (high, left)
  //
  // . ordinary value
  // - ordinary function
  // + lifted value
  // ~ lifted function
  // > lifting function
  module Infix: {
    /* sequence lifted value and lifted value */
    let ($++): (t('a), t('b)) => t('b);

    /* sequence lifted value and ordinary value */
    let ($+.): (t('a), 'b) => t('b);

    /* sequence ordinary value and lifted value */
    let ($.+): ('a, t('b)) => t('b);

    /* sequence ordinary value and ordinary value */
    let ($..): ('a, 'b) => t('b);

    /* bind lifted value */
    let (@+>): (t('a), 'a => t('b)) => t('b);
    let (@>+): ('a => t('b), t('a)) => t('b);

    /* bind ordinary value */
    let (@.>): ('a, 'a => t('b)) => t('b);
    let (@>.): ('a => t('b), 'a) => t('b);

    /* apply lifted function to lifted value (low, right) */
    let (^~+): (t('a => 'b), t('a)) => t('b);
    let (^+~): (t('a), t('a => 'b)) => t('b);

    /* apply lifted function to ordinary value (low, right) */
    let (^~.): (t('a => 'b), 'a) => t('b);
    let (^.~): ('a, t('a => 'b)) => t('b);

    /* apply ordinary function to lifted value (low, right) */
    let (^-+): ('a => 'b, t('a)) => t('b);
    let (^+-): (t('a), 'a => 'b) => t('b);

    /* apply ordinary function to ordinary value (low, right) */
    let (^-.): ('a => 'b, 'a) => t('b);
    let (^.-): ('a, 'a => 'b) => t('b);

    /* apply lifted function to lifted value (mid, left) */
    let (+~+): (t('a => 'b), t('a)) => t('b);
    let (++~): (t('a), t('a => 'b)) => t('b);

    /* apply lifted function to ordinary value (mid, left) */
    let (+~.): (t('a => 'b), 'a) => t('b);
    let (+.~): ('a, t('a => 'b)) => t('b);

    /* apply ordinary function to lifted value (mid, left) */
    let (+-+): ('a => 'b, t('a)) => t('b);
    let (++-): (t('a), 'a => 'b) => t('b);

    /* apply ordinary function to ordinary value (mid, left) */
    let (+-.): ('a => 'b, 'a) => t('b);
    let (+.-): ('a, 'a => 'b) => t('b);

    /* compose ordinary function with ordinary function */
    let (%--): ('b => 'c, 'a => 'b, t('a)) => t('c);

    /* compose ordinary function with lifted function */
    let (%-~): ('b => 'c, t('a => 'b), t('a)) => t('c);

    /* compose lifted function with ordinary function */
    let (%~-): (t('b => 'c), 'a => 'b, t('a)) => t('c);

    /* compose lifted function with lifted function */
    let (%~~): (t('b => 'c), t('a => 'b), t('a)) => t('c);

    /* apply lifted function to lifted value (high, left) */
    let ( **~+ ): (t('a => 'b), t('a)) => t('b);
    let ( **+~ ): (t('a), t('a => 'b)) => t('b);

    /* apply lifted function to ordinary value (high, left) */
    let ( **~. ): (t('a => 'b), 'a) => t('b);
    let ( **.~ ): ('a, t('a => 'b)) => t('b);

    /* apply ordinary function to lifted value (high, left) */
    let ( **-+ ): ('a => 'b, t('a)) => t('b);
    let ( **+- ): (t('a), 'a => 'b) => t('b);

    /* apply ordinary function to ordinary value (high, left) */
    let ( **-. ): ('a => 'b, 'a) => t('b);
    let ( **.- ): ('a, 'a => 'b) => t('b);
  };
};

module Make: (M: MONAD_BASIC) => MONAD with type t('a) := M.t('a);

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

  let get: unit => t(state);
  let put: state => t(unit);
};

module State: {
  module Make:
    (M: STATE_MONAD_BASIC) =>
     STATE_MONAD with type state = M.state and type t('result) = M.t('result);
};
