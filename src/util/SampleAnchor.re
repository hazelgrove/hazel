/* Gesture-keyed one-shot horizontal follow for Left/Right sample nav:
 * capture the target at keydown, then after the next render scroll it into
 * a comfort band. Gesture-keyed so it never fires on unrelated re-renders.
 * Vertical shifts (drawers reflowing above the probe) are RefractorShift's. */

open Js_of_ocaml;

/* Caret-adjacent sample, for the focus-bar arrows. Probe-level arrows pass
 * ~scope/~sample_id instead, since the arrowed probe may not be caret-adjacent. */
let default_selector = ".projector.probe.indicated .sample.indicated-sample";

let selector_for = (~scope: option(string), ~sample_id: option(int)) =>
  switch (scope, sample_id) {
  | (Some(dom_id), Some(sid)) =>
    Printf.sprintf("#%s .sample[data-sample-id='%d']", dom_id, sid)
  | (Some(dom_id), None) => "#" ++ dom_id ++ " .sample.indicated-sample"
  | (None, _) => default_selector
  };

/* `fallback`: the probe's indicated sample, for when the exact target
 * isn't what rendered. */
type anchor = {
  primary: string,
  fallback: string,
};

let pending: ref(option(anchor)) = ref(None);

let find = (sel: string): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(Dom_html.document##querySelector(Js.string(sel)));

let find_anchor = (a: anchor): option(Js.t(Dom_html.element)) =>
  switch (find(a.primary)) {
  | Some(el) => Some(el)
  | None => find(a.fallback)
  };

let capture =
    (~scope: option(string)=?, ~sample_id: option(int)=?, ()): unit =>
  pending :=
    Some({
      primary: selector_for(~scope, ~sample_id),
      fallback: selector_for(~scope, ~sample_id=None),
    });

/* Bring the indicated sample into a comfort band, minimal motion. Margin
 * adapts to width: small samples get a lookahead preview; one that fits is
 * shown fully; one wider than the viewport aligns its left edge (read from
 * the start). scrollLeft set directly — smooth scrolling queues badly. */
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
      let m = Float.max(pad, Float.min(lookahead, width -. el_width -. pad));
      let delta =
        if (el_width > width -. 2. *. pad) {
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
      };
    },
  );
};

let consume = (): unit =>
  switch (pending^) {
  | None => ()
  | Some(a) =>
    pending := None;
    Option.iter(scroll_horizontally, find_anchor(a));
  };
