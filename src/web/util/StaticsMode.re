/* Whether a frame's statics recompute runs normally, is deferred by the typing
 * debounce, or is forced by the debounce timer firing.
 *
 * Produced by CodeWithStatics.StaticsDebounce.consume and consumed both by
 * CodeWithStatics.Update.calculate (which decides whether to recompute) and by
 * PerfMetrics (which reports the outcome in the Statics panel). It lives in its
 * own module so the collector can name the decision without depending on the
 * editor. */
type t =
  | Normal
  | Defer
  | Force;
