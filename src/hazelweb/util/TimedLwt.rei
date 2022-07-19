type t('a) = Lwt.t('a);

/**
  [wrap t q] wraps [q] in a timeout of [t] milliseconds, giving [q']. [q']
  yields [Some(x)] when [q] completes within the timeout and [None] otherwise.
 */
let wrap: (int, Lwt.t('a)) => Lwt.t(option('a));
