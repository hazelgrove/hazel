/**
  Timeout wrapper for {!Lwt.t}.
 */
open Lwt;

module type TIMER = {
  /**
    [delay f t] applies [f ()] after [t] milliseconds.
   */
  let delay: (unit => unit, int) => unit;
};

module type S = {
  /**
    [wrap t q] wraps [q] in a timeout of [t] milliseconds, giving [q']. [q']
    yields [Some(x)] when [q] completes within the timeout and [None]
    otherwise.
   */
  let wrap: (int, t('a)) => t(option('a));
};

[@warning "-67"]
module Make: (T: TIMER) => S;
