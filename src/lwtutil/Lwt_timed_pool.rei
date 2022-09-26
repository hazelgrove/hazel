/**
  Resource pool with timeouts, inspired by {!Lwt.Lwt_pool}.
 */

/** Create a fresh pool member. */
type create('a) = unit => Lwt.t('a);
/** Dispose of a pool member. */
type dispose('a) = 'a => Lwt.t(unit);
/** Check the validity of a member when [use] resulted in a failed computation. */
type check('a) = 'a => Lwt.t(bool);
/** Validate an existing available member before use. */
type validate('a) = 'a => Lwt.t(bool);

module type S = {
  /**
    The type for a timed pool.
   */
  type t('a);

  /**
    [init ~max ~create ~dispose ~check ~validate] is a pool with [max] members.

    See {!type:create}, {!type:dispose}, {!type:check}, {!type:validate}, and
    {!module:Lwt_pool}.
   */
  let init:
    (
      ~max: int,
      ~create: create('a),
      ~dispose: dispose('a),
      ~check: check('a),
      ~validate: validate('a)
    ) =>
    t('a);

  /**
    [use pool t f] retrieves and available pool member and applies [f] with the
    timeout of [t] milliseconds, giving [q]. If [f] exceeds the timeout, [q]
    yields [None].

    See also {!module:TimedLwt}.
   */
  let use: (t('a), int, 'a => (Lwt.t('b), 'a)) => Lwt.t(option('b));

  /**
    [add pool] creates a fresh pool member if capacity is available and releases
    as available, returning [Lwt.return_true] if succeeded.
   */
  let add: t('a) => Lwt.t(bool);

  /**
    [fill pool n] creates [n] fresh pool members or until capacity is reached.
   */
  let fill: (t('a), int) => Lwt.t(unit);

  /**
    [clear pool] clears all members of the pool.
   */
  let clear: t('a) => Lwt.t(unit);
};

module Make: (Lwt_timed.S) => S;
