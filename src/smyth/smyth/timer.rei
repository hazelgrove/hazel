/** A collection of timers used to cut off program execution early. */;

/** A countdown timer that, once started, continues to completion. */

module Single: {
  /** The available single timers. */

  type t =
    | /** The timer for the total synthesis time. */
      Total
    | /** The timer for live evaluation and resumption. */
      Eval;

  /** Starts the timer. */

  let start: t => unit;

  /** Returns how much time has elapsed since starting the timer. */

  let elapsed: t => float;

  /** Returns [true] if the timer is still running, and [false] if the elapsed
      time is greater than the cutoff. */

  let check: t => bool;
};

/** A resettable countdown timer that accumulates time every time the {!accumulate}
    function is called. */

module Multi: {
  /** The available multi timers. */

  type t =
    | /** The timer for raw term enumeration. */
      Guess;

  /** Resets the timer. */

  let reset: t => unit;

  /** Accumulates additional time on the timer. */

  let accumulate: (t, unit => 'a) => 'a;

  /** Returns [true] if the timer is still running, and [false] if the elapsed
      time is greater than the cutoff. */

  let check: t => bool;
};

/** A fragile hard-cutoff timer that uses Unix itimers. */

let itimer_timeout: (string, float, 'a => 'b, 'a, 'b) => ('b, float, bool);
