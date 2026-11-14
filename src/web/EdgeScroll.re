open Js_of_ocaml;
open Util;

/* Edge-scrolling during drag-select: when the pointer is near an edge
 * of a scroll container, automatically scroll in that direction at a
 * speed proportional to how close the pointer is to the edge. #main
 * scrolls up and down; the code scroller the drag is in (see
 * JsUtil.CodeScroll) scrolls sideways. A setInterval timer drives the
 * scrolling so it continues even when the mouse is stationary at the edge.
 *
 * On each tick, after scrolling, the on_tick callback is invoked to
 * update the selection. This keeps the selection in sync with the
 * scroll even when the mouse is stationary. */

let margin_px = 48.0;
let max_speed_px = 35.0;
let interval_ms = 10.0;

let timer_id: ref(option(Dom_html.interval_id)) = ref(None);
let last_client_x: ref(float) = ref(0.0);
let last_client_y: ref(float) = ref(0.0);
let code_scroller: ref(option(Js.t(Dom_html.element))) = ref(None);
let on_tick: ref(option(unit => unit)) = ref(None);

let is_active = (): bool => timer_id^ != None;

let clear_timer = () =>
  switch (timer_id^) {
  | Some(id) =>
    Dom_html.window##clearInterval(id);
    timer_id := None;
  | None => ()
  };

let stop = () => {
  clear_timer();
  last_client_x := 0.0;
  last_client_y := 0.0;
  code_scroller := None;
  on_tick := None;
};

/* Pixels per tick to scroll for a pointer at `pos` between the edges `lo`
 * and `hi`: negative toward lo, positive toward hi. */
let speed = (~lo: float, ~hi: float, pos: float): float => {
  let dist_from_lo = pos -. lo;
  let dist_from_hi = hi -. pos;
  if (dist_from_lo < 0.0) {
    -. max_speed_px;
  } else if (dist_from_hi < 0.0) {
    max_speed_px;
  } else if (dist_from_lo < margin_px) {
    let ratio = 1.0 -. dist_from_lo /. margin_px;
    -. max_speed_px *. ratio;
  } else if (dist_from_hi < margin_px) {
    let ratio = 1.0 -. dist_from_hi /. margin_px;
    max_speed_px *. ratio;
  } else {
    0.0;
  };
};

let compute_delta_y = (client_y: float): float =>
  switch (JsUtil.get_elem_by_id_opt("main")) {
  | None => 0.0
  | Some(container) =>
    let rect = container##getBoundingClientRect;
    speed(~lo=rect##.top, ~hi=rect##.bottom, client_y);
  };

/* Zero once the code has no further to scroll that way: most code fits
 * its scroller, and a drag near its edges shouldn't keep the timer going. */
let compute_delta_x = (client_x: float): float =>
  switch (code_scroller^) {
  | None => 0.0
  | Some(scroller) =>
    let (left, right) = JsUtil.CodeScroll.visible_x(scroller);
    let delta = speed(~lo=left, ~hi=right, client_x);
    let scroll_left = scroller##.scrollLeft;
    /* less a pixel: scrollLeft is truncated from a sub-pixel position */
    let scroll_max = scroller##.scrollWidth - scroller##.clientWidth - 1;
    let can_scroll = delta < 0.0 ? scroll_left > 0 : scroll_left < scroll_max;
    can_scroll ? delta : 0.0;
  };

let tick = () => {
  let delta_x = compute_delta_x(last_client_x^);
  let delta_y = compute_delta_y(last_client_y^);
  if (delta_x != 0.0 || delta_y != 0.0) {
    Option.iter(JsUtil.CodeScroll.scroll_by(_, delta_x), code_scroller^);
    Option.iter(
      JsUtil.adjust_scroll(_, delta_y),
      JsUtil.get_elem_by_id_opt("main"),
    );
    switch (on_tick^) {
    | Some(f) => f()
    | None => ()
    };
  };
};

let ensure_timer = () =>
  switch (timer_id^) {
  | Some(_) => ()
  | None =>
    let id =
      Dom_html.window##setInterval(
        Js.wrap_callback(() => tick()),
        interval_ms,
      );
    timer_id := Some(id);
  };

/* `container` is the code container the drag is in. */
let update =
    (
      ~container: Js.t(Dom_html.element),
      ~client_x: float,
      ~client_y: float,
      ~on_scroll: unit => unit,
    ) => {
  last_client_x := client_x;
  last_client_y := client_y;
  code_scroller := JsUtil.CodeScroll.containing(container);
  on_tick := Some(on_scroll);
  if (compute_delta_x(client_x) != 0.0 || compute_delta_y(client_y) != 0.0) {
    ensure_timer();
  } else {
    clear_timer();
  };
};
