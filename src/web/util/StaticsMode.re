/* Whether a frame's statics recompute runs normally, is deferred by the typing
 * debounce, or is forced by the debounce timer firing. Produced by
 * CodeWithStatics.StaticsDebounce.consume; it sits in its own module so
 * PerfMetrics can name the decision without depending on the editor. */
type t =
  | Normal
  | Defer
  | Force;
