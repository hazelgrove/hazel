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

/* Default anchor: the caret-adjacent probe's aligned sample — right for
 * the sample focus bar, whose arrows navigate the global focus. Probe-
 * level arrows must NOT use it: the user can arrow through a probe that
 * is not caret-adjacent, and alignment moves other probes' aligned
 * samples in sympathy, so the default selector tracks (and follows) the
 * wrong probe's sample. Those callers pass ~scope with the gesture's
 * probe DOM id, anchoring the sample inside that probe specifically. */
let default_selector = ".projector.probe.indicated .sample.indicated-sample";

let selector_for = (~scope: option(string)) =>
  switch (scope) {
  | Some(dom_id) => "#" ++ dom_id ++ " .sample.indicated-sample"
  | None => default_selector
  };

/* Pending anchor: the target selector and its rect at capture time,
 * plus a frame budget. consume() fires from EVERY after_display,
 * including renders that happen between the keydown and the render that
 * actually applies the action (e.g. a settling reflow from the previous
 * press). Spending the anchor on such an early frame measures the
 * PRE-action DOM, so consume holds the anchor while the measured rect
 * is unchanged from capture time (capture only fires when the
 * indication will move); the frame budget expires stale anchors so they
 * can't fire on some later unrelated render. */
type anchor = {
  sel: string,
  top: float,
  left: float,
  right: float,
  mutable frames_left: int,
};

let pending: ref(option(anchor)) = ref(None);

let read_rect = (sel: string): option((float, float, float)) => {
  let doc = Dom_html.document;
  Js.Opt.case(
    doc##querySelector(Js.string(sel)),
    () => None,
    el => {
      let rect = el##getBoundingClientRect;
      Some((rect##.top, rect##.left, rect##.right));
    },
  );
};

let capture = (~scope: option(string)=?, ()): unit => {
  let sel = selector_for(~scope);
  switch (read_rect(sel)) {
  | None => pending := None
  | Some((top, left, right)) =>
    pending :=
      Some({
        sel,
        top,
        left,
        right,
        frames_left: 3,
      });
    ScrollDebug.log(
      "SA",
      Printf.sprintf("capture top=%.1f left=%.1f sel=%s", top, left, sel),
    );
  };
};

/* Horizontal comfort band: after Left/Right moves the indication, bring
 * the indicated sample minimally into view in #main, leaving a margin
 * on the violated side. The margin ADAPTS to the sample's width:
 *   - small samples get the full lookahead margin (10% of the viewport,
 *     mirroring the vertical helpers' margin_ratio), which previews the
 *     next sample in the direction of travel;
 *   - the margin shrinks as samples grow, down to a small pad, so a
 *     sample that FITS the viewport is always brought FULLY into view
 *     (never tail-clipped for the sake of preview);
 *   - a sample wider than the viewport itself aligns its left edge: a
 *     wide value is read from its start, so "nearest edge" could strand
 *     the view on its tail.
 * Minimal motion means no scroll at all while the sample is already
 * comfortably visible. One-shot and gesture-keyed exactly like the
 * vertical compensation (capture only fires when the indication
 * actually moves), so it never runs on unrelated re-renders and cannot
 * hijack manual scrolling. scrollLeft is set directly (instant): smooth
 * scrolling queues badly under repeated key presses. */
let scroll_horizontally = (sel: string): unit => {
  let doc = Dom_html.document;
  Js.Opt.iter(doc##querySelector(Js.string(sel)), el =>
    Js.Opt.iter(
      doc##getElementById(Js.string("main")),
      main => {
        let el_rect = el##getBoundingClientRect;
        let main_rect = main##getBoundingClientRect;
        let vp_left = main_rect##.left;
        let vp_right = main_rect##.right;
        let width = vp_right -. vp_left;
        let pad = 16.;
        let lookahead = width *. 0.10;
        let el_width = el_rect##.right -. el_rect##.left;
        /* Margin for the violated side: as much preview as fits. */
        let m =
          Float.max(pad, Float.min(lookahead, width -. el_width -. pad));
        let delta =
          if (el_width > width -. 2. *. pad) {
            /* Wider than the viewport can show: align its start. */
            el_rect##.left -. (vp_left +. pad);
          } else if (el_rect##.left < vp_left +. m) {
            el_rect##.left -. (vp_left +. m);
          } else if (el_rect##.right > vp_right -. m) {
            el_rect##.right -. (vp_right -. m);
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
          let sl_after: float = Js.Unsafe.get(main, Js.string("scrollLeft"));
          let s_w: int = Js.Unsafe.get(main, Js.string("scrollWidth"));
          let c_w: int = Js.Unsafe.get(main, Js.string("clientWidth"));
          /* TEMP diag: el box vs viewport vs scroll extent. Remove. */
          print_endline(
            Printf.sprintf(
              "[HSCROLL] el=[%.0f,%.0f] (w=%.0f) vp=[%.0f,%.0f] m=%.0f dx=%+.0f sl=%.0f->%.0f max=%d",
              el_rect##.left,
              el_rect##.right,
              el_width,
              vp_left,
              vp_right,
              m,
              delta,
              sl,
              sl_after,
              s_w - c_w,
            ),
          );
        };
      },
    )
  );
};

let consume = (): unit =>
  switch (pending^) {
  | None => () /* nothing pending: silent (the common case) */
  | Some(a) =>
    switch (read_rect(a.sel)) {
    | None => pending := None
    | Some((new_top, new_left, new_right)) =>
      let unchanged =
        Float.abs(new_top -. a.top) < 0.5
        && Float.abs(new_left -. a.left) < 0.5
        && Float.abs(new_right -. a.right) < 0.5;
      if (unchanged && a.frames_left > 0) {
        /* The action hasn't rendered yet (this after_display belongs to
         * an earlier, unrelated render). Hold the anchor. */
        a.frames_left = a.frames_left - 1;
        ScrollDebug.log("SA", "consume held (rect unchanged)");
      } else {
        pending := None;
        let delta = new_top -. a.top;
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
              a.top,
              new_top,
            ),
          );
          ScrollDebug.mark_sT();
        };
        /* Horizontal follow runs whenever the gesture fired, even when
         * the vertical delta was zero (the common case in many mode). */
        scroll_horizontally(a.sel);
      };
    }
  };
