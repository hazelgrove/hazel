/* Gesture-keyed scroll behavior for Left/Right sample navigation:
 * one-shot anchors captured at keydown and consumed after the next
 * render (Main.after_display). Two effects per gesture:
 *
 *   - VERTICAL compensation: the action can change drawer heights
 *     above the anchored sample and reflow it on screen; consume
 *     scrolls #main vertically by the rect.top delta so the sample
 *     stays put under the user's eye.
 *   - HORIZONTAL follow: in many mode the newly indicated sample can
 *     sit off-screen; consume scrolls #main horizontally to bring it
 *     into a comfort band (see scroll_horizontally).
 *
 * Callers: ProbeProj.move_cursor (probe-level arrows; passes ~scope +
 * ~sample_id to anchor the exact sample the gesture selected) and the
 * sample focus bar arrows (default caret-adjacent anchor). Both call
 * capture() only when the gesture will actually change state, so
 * anchors never fire on no-op presses or unrelated re-renders. Uses
 * float scroll positions to avoid sub-pixel drift across repeated
 * presses. */

open Js_of_ocaml;

/* Default anchor: the caret-adjacent probe's aligned sample
 * (`.indicated` marks the unique probe adjacent to the caret) — right
 * for the sample focus bar, whose arrows navigate the global focus.
 * Probe-level arrows must NOT use it: the user can arrow through a
 * probe that is not caret-adjacent, so those callers pass ~scope (the
 * gesture's probe DOM id) and ~sample_id (its data-sample-id),
 * anchoring the exact element the gesture targeted. */
let default_selector = ".projector.probe.indicated .sample.indicated-sample";

let selector_for = (~scope: option(string), ~sample_id: option(int)) =>
  switch (scope, sample_id) {
  | (Some(dom_id), Some(sid)) =>
    Printf.sprintf("#%s .sample[data-sample-id='%d']", dom_id, sid)
  | (Some(dom_id), None) => "#" ++ dom_id ++ " .sample.indicated-sample"
  | (None, _) => default_selector
  };

/* Pending anchor: rect of the gesture's target element at capture
 * time. `primary` is the id-precise selector (probe + data-sample-id)
 * when the caller knows its target; `fallback` is the class-based
 * selector, used when the primary matches nothing — e.g. in Single
 * window mode the target sample is not in the DOM until the action
 * renders, so capture measures the in-place predecessor and consume
 * finds the target by id afterward. */
type anchor = {
  primary: string,
  fallback: string,
  top: float,
  left: float,
  right: float,
};

let pending: ref(option(anchor)) = ref(None);

let find = (sel: string): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(Dom_html.document##querySelector(Js.string(sel)));

let find_anchor = (a: anchor): option(Js.t(Dom_html.element)) =>
  switch (find(a.primary)) {
  | Some(el) => Some(el)
  | None => find(a.fallback)
  };

let rect_of = (el: Js.t(Dom_html.element)): (float, float, float) => {
  let r = el##getBoundingClientRect;
  (r##.top, r##.left, r##.right);
};

let capture =
    (~scope: option(string)=?, ~sample_id: option(int)=?, ()): unit => {
  let primary = selector_for(~scope, ~sample_id);
  let fallback = selector_for(~scope, ~sample_id=None);
  let a = {
    primary,
    fallback,
    top: 0.,
    left: 0.,
    right: 0.,
  };
  switch (find_anchor(a)) {
  | None => pending := None
  | Some(el) =>
    let (top, left, right) = rect_of(el);
    pending :=
      Some({
        ...a,
        top,
        left,
        right,
      });
    ScrollDebug.log(
      "SA",
      Printf.sprintf("capture top=%.1f left=%.1f sel=%s", top, left, primary),
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
let scroll_horizontally = (el: Js.t(Dom_html.element)): unit => {
  let doc = Dom_html.document;
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
      let m = Float.max(pad, Float.min(lookahead, width -. el_width -. pad));
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
        ScrollDebug.log(
          "SA",
          Printf.sprintf("consume h-SCROLLED dx=%+.1f", delta),
        );
      };
    },
  );
};

let consume = (): unit =>
  switch (pending^) {
  | None => () /* nothing pending: silent (the common case) */
  | Some(a) =>
    pending := None;
    switch (find_anchor(a)) {
    | None => ()
    | Some(el) =>
      let (new_top, _, _) = rect_of(el);
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
      scroll_horizontally(el);
    };
  };
