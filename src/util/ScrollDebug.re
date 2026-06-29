/* Lightweight logging for the scroll subsystem (EdgeScroll, CaretAnchor,
 * SampleAnchor, scroll-into-view, focus-bar compensation). Output goes to
 * the JS console as single-line entries:
 *
 *   [F<frame> +<dt-ms> <tag>] <message>
 *
 * - frame: monotonic counter incremented at the start of every
 *   Main.after_display. Lets you group events by render frame.
 * - dt-ms: milliseconds since the previous log entry. Lets you spot
 *   high-frequency activity (e.g. EdgeScroll ticking at 10ms intervals)
 *   and lulls (between drags).
 * - tag: 2-3 char subsystem code:
 *     AF = after_display frame entry
 *     ES = EdgeScroll
 *     CA = CaretAnchor
 *     SA = SampleAnchor
 *     SI = scroll_cursor_into_view_if_needed
 *     FB = focus-bar resize observer
 *
 * Toggle via `window.SCROLL_DEBUG = false` in devtools to silence. */

open Js_of_ocaml;

/* Off by default. Enable from devtools with `window.SCROLL_DEBUG = true`
 * — the toggle is rechecked on every call so no reload is needed. */
let enabled: ref(bool) = ref(false);
let frame: ref(int) = ref(0);
let last_t: ref(float) = ref(0.0);

let now_ms = (): float => {
  let perf = Js.Unsafe.global##.performance;
  let n: float = Js.Unsafe.meth_call(perf, "now", [||]);
  n;
};

/* Re-check the JS-side toggle each call so the user can flip it without
 * a reload via `window.SCROLL_DEBUG = false`. */
let check_enabled = () => {
  let g = Js.Unsafe.global;
  let v: Js.Optdef.t(Js.t(Js.js_string)) =
    Js.Unsafe.get(g, Js.string("SCROLL_DEBUG"));
  switch (Js.Optdef.to_option(v)) {
  | None => enabled^
  | Some(b) =>
    /* truthy check via JS coercion */
    let truthy: bool =
      Js.to_bool(
        Js.Unsafe.fun_call(
          Js.Unsafe.js_expr("Boolean"),
          [|Js.Unsafe.inject(b)|],
        ),
      );
    truthy;
  };
};

let next_frame = (): unit =>
  if (check_enabled()) {
    incr(frame);
  };

let frame_no = (): int => frame^;

let log = (tag: string, msg: string): unit =>
  if (check_enabled()) {
    let t = now_ms();
    let dt =
      if (last_t^ == 0.0) {
        0.0;
      } else {
        t -. last_t^;
      };
    last_t := t;
    let prefix = Printf.sprintf("[F%04d +%6.1fms %s] ", frame^, dt, tag);
    Firebug.console##log(Js.string(prefix ++ msg));
  };

/* Read current scrollTop of #main, for logging context. */
let main_scroll_top = (): float =>
  switch (
    Js.Opt.to_option(Dom_html.document##getElementById(Js.string("main")))
  ) {
  | None => Float.nan
  | Some(main) => Js.Unsafe.get(main, Js.string("scrollTop"))
  };

/* Drift detector: maintained by callers. Each scroll-changing event
 * (EdgeScroll, CaretAnchor, SampleAnchor, SI, FB) updates this *after*
 * its own scroll mutation. The AF frame start compares actual
 * #main.scrollTop against this; any non-zero diff is "unaccounted drift"
 * — most likely Chrome's native CSS scroll-anchoring (we don't set
 * overflow-anchor:none) but could also be browser-scrollbar interaction,
 * etc. */
let last_known_sT: ref(float) = ref(Float.nan);

let mark_sT = (): unit => last_known_sT := main_scroll_top();

/* Caller passes an "in-drag" predicate (EdgeScroll.is_active). Outside a
 * drag, wheel/trackpad scrolling would dominate the log; we only care
 * about unaccounted drift during the drag-select window the user is
 * investigating. Large drifts (>20px) are always logged on the assumption
 * that those are interesting regardless. */
let check_drift = (~in_drag: bool=false, ()): unit => {
  let actual = main_scroll_top();
  let prev = last_known_sT^;
  if (!Float.is_nan(prev) && !Float.is_nan(actual) && actual != prev) {
    let delta = actual -. prev;
    if (in_drag || Float.abs(delta) > 20.0) {
      log(
        "DR",
        Printf.sprintf(
          "DRIFT sT %.1f -> %.1f (delta %+.1f)",
          prev,
          actual,
          delta,
        ),
      );
    };
    /* Always refresh baseline so we don't keep re-logging the same drift. */
    last_known_sT := actual;
  };
};
