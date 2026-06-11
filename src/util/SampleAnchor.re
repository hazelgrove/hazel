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

/* Horizontal comfort band: after Left/Right moves the indication, bring
 * the indicated sample minimally into [left+M, right-M] of #main's
 * viewport, M = 10% of its width (mirroring the vertical helpers'
 * margin_ratio). Minimal motion means no scroll at all while samples
 * are already comfortably visible, and the M margin previews the next
 * sample in the direction of travel. A sample WIDER than the band
 * (drawer mode) aligns its left edge to the band's left instead: a wide
 * value is read from its start, so "nearest edge" could strand the view
 * on its tail. One-shot and gesture-keyed exactly like the vertical
 * compensation, so it never runs on unrelated re-renders and cannot
 * hijack manual scrolling. scrollLeft is set directly (instant): smooth
 * scrolling queues badly under repeated key presses. */
let scroll_horizontally = (): unit => {
  let doc = Dom_html.document;
  Js.Opt.iter(doc##querySelector(Js.string(selector)), el =>
    Js.Opt.iter(
      doc##getElementById(Js.string("main")),
      main => {
        let el_rect = el##getBoundingClientRect;
        let main_rect = main##getBoundingClientRect;
        let width = Js.Optdef.get(main_rect##.width, _ => 0.);
        let margin = width *. 0.10;
        let band_left = main_rect##.left +. margin;
        let band_right = main_rect##.right -. margin;
        let el_width = el_rect##.right -. el_rect##.left;
        let delta =
          if (el_width > band_right -. band_left || el_rect##.left < band_left) {
            /* Oversize: align start. Undersize, off to the left: bring
             * its left edge to the band's left. Same correction. */
            el_rect##.left -. band_left;
          } else if (el_rect##.right > band_right) {
            el_rect##.right -. band_right;
          } else {
            0.;
          };
        if (delta != 0.0) {
          let sl: float = Js.Unsafe.get(main, Js.string("scrollLeft"));
          Js.Unsafe.set(
            main,
            Js.string("scrollLeft"),
            Float.max(0., sl +. delta),
          );
          ScrollDebug.log(
            "SA",
            Printf.sprintf("consume h-SCROLLED dx=%+.1f", delta),
          );
        };
      },
    )
  );
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
    /* Horizontal follow runs whenever the gesture fired, even when the
     * vertical delta was zero (the common case in many mode). */
    scroll_horizontally();
  };
