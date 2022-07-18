/**
  Statistics about the current program evaluation.

  This currently only holds the step count, but it may be useful for holding
  additional information (e.g., how many times it has been resumed w/ fill-and-resume).
 */

/**
  The type for evaluation statistics.
 */
[@deriving sexp]
type t;

/**
  [initial] is the initial statistics.
 */
let initial: t;

/**
  [take_step stats] is [stats] with an incremented step count.
 */
let take_step: t => t;

/**
  [get_step stats] is the step count.
 */
let get_step: t => int;
