/* Sample-focus anchor scroll compensation for Left/Right in the
 * sample focus bar.
 *
 * When the user presses Left/Right in #sample-focus-bar to move the
 * dynamic cursor (Project(SampleFocus(SetIndex(_)))), the displayed
 * sample in each probe can change. In Window=Single mode this can
 * change drawer heights of probes above the indicated probe and
 * reflow the indicated sample on screen.
 *
 * To keep the indicated sample's screen position stable:
 *
 *   1. `capture`: synchronously in the keydown handler, before the
 *      action dispatches, read the indicated sample element's
 *      `getBoundingClientRect().top` and stash it.
 *   2. `consume` (in Main.after_display, after the next render): read
 *      the new indicated sample element's rect.top and scroll #main
 *      by the delta. Uses float scrollTop to avoid sub-pixel drift
 *      across repeated arrow presses.
 *
 * Anchor element: `.projector.probe.indicated .sample.indicated-sample`.
 * `.indicated` is on the (unique) probe adjacent to the caret;
 * `.indicated-sample` is on each probe's `most_aligned_sample`. The
 * combined selector matches exactly one element when both are present.
 * If the selector matches nothing (no indicated probe, or no aligned
 * sample), capture/consume are no-ops. */

open Js_of_ocaml;

let selector = ".projector.probe.indicated .sample.indicated-sample";

let pending: ref(option(float)) = ref(None);

let read_top = (): option(float) => {
  let doc = Dom_html.document;
  Js.Opt.case(
    doc##querySelector(Js.string(selector)),
    () => None,
    el => {
      let rect = el##getBoundingClientRect;
      Some(rect##.top);
    },
  );
};

let capture = (): unit => {
  let v = read_top();
  pending := v;
  switch (v) {
  | Some(y) => ScrollDebug.log("SA", Printf.sprintf("capture top=%.1f", y))
  | None => ()
  };
};

let consume = (): unit =>
  switch (pending^) {
  | None => () /* nothing pending: silent (the common case) */
  | Some(old_top) =>
    pending := None;
    switch (read_top()) {
    | None => ()
    | Some(new_top) =>
      let delta = new_top -. old_top;
      if (delta != 0.0) {
        let doc = Dom_html.document;
        Js.Opt.iter(
          doc##getElementById(Js.string("main")),
          main => {
            let st: float = Js.Unsafe.get(main, Js.string("scrollTop"));
            Js.Unsafe.set(main, Js.string("scrollTop"), st +. delta);
          },
        );
        ScrollDebug.log(
          "SA",
          Printf.sprintf(
            "consume SCROLLED dy=%+.1f (old=%.1f new=%.1f)",
            delta,
            old_top,
            new_top,
          ),
        );
        ScrollDebug.mark_sT();
      };
    };
  };
