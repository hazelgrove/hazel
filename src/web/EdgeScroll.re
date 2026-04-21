open Js_of_ocaml;
open Util;

/* Edge-scrolling during drag-select: when the pointer is near the
 * top or bottom edge of the scroll container, automatically scroll
 * in that direction at a speed proportional to how close the pointer
 * is to the edge. A setInterval timer drives the scrolling so it
 * continues even when the mouse is stationary at the edge.
 *
 * On each tick, after scrolling, the on_tick callback is invoked to
 * update the selection. This keeps the selection in sync with the
 * scroll even when the mouse is stationary. */

let margin_px = 48.0;
let max_speed_px = 35.0;
let interval_ms = 10.0;

let timer_id: ref(option(Dom_html.interval_id)) = ref(None);
let last_client_y: ref(float) = ref(0.0);
let on_tick: ref(option(unit => unit)) = ref(None);

let clear_timer = () =>
  switch (timer_id^) {
  | Some(id) =>
    Dom_html.window##clearInterval(id);
    timer_id := None;
  | None => ()
  };

let stop = () => {
  clear_timer();
  last_client_y := 0.0;
  on_tick := None;
};

let compute_delta = (client_y: float): float =>
  switch (JsUtil.get_elem_by_id_opt("main")) {
  | None => 0.0
  | Some(container) =>
    let rect = container##getBoundingClientRect;
    let top = rect##.top;
    let bottom = rect##.bottom;
    let dist_from_top = client_y -. top;
    let dist_from_bottom = bottom -. client_y;
    if (dist_from_top < 0.0) {
      -. max_speed_px;
    } else if (dist_from_bottom < 0.0) {
      max_speed_px;
    } else if (dist_from_top < margin_px) {
      let ratio = 1.0 -. dist_from_top /. margin_px;
      -. max_speed_px *. ratio;
    } else if (dist_from_bottom < margin_px) {
      let ratio = 1.0 -. dist_from_bottom /. margin_px;
      max_speed_px *. ratio;
    } else {
      0.0;
    };
  };

let tick = () =>
  switch (JsUtil.get_elem_by_id_opt("main")) {
  | None => ()
  | Some(container) =>
    let delta = compute_delta(last_client_y^);
    if (delta != 0.0) {
      JsUtil.adjust_scroll(container, delta);
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

let update = (~client_y: float, ~on_scroll: unit => unit) => {
  last_client_y := client_y;
  on_tick := Some(on_scroll);
  let delta = compute_delta(client_y);
  if (delta != 0.0) {
    ensure_timer();
  } else {
    clear_timer();
  };
};
