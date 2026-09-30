/* caret reveal without forced layout while typing. the caret's row is
   model data (CaretDec publishes (row, row_height) at render); only the
   editor's origin in its scroll container and the scroll state need the
   DOM. a reveal after a pause reads them into mirrors; reveals
   within a burst are arithmetic plus a scrollTop write. a
   throttled rAF check re-reads ground truth and heals drift */

open Js_of_ocaml;

let margin_ratio = 0.10; /* trigger band, fraction of viewport height
                            (matches scroll_vertically_into_view) */

let burst_ms = 500.;
let verify_min_gap_ms = 150.;
let heal_tolerance_px = 2.;

type geom = {
  container: Js.t(Dom_html.element),
  /* the caret node the anchor was read against: moving to another
     editor mints a new caret node, and published rows are
     editor-relative, so a burst reveal requires this same node */
  caret_el: Js.t(Dom_html.element),
  /* content-space y of the active editor's row 0 */
  mutable editor_top: float,
  mutable height: float,
  /* mirror of container##.scrollTop: our writes plus a scroll listener,
     which also catches programmatic writes */
  mutable scroll_top: float,
};

let published: ref(option((int, float))) = ref(None);
let publish = (~row: int, ~row_height: float): unit =>
  published := Some((row, row_height));

let geom: ref(option(geom)) = ref(None);
let last_reveal_ms: ref(float) = ref(0.);
let last_verify_ms: ref(float) = ref(0.);
let verify_scheduled = ref(false);

let now_ms = (): float => Js.Unsafe.global##.Date##now();

let connected = (el: Js.t(Dom_html.element)): bool =>
  try(Js.to_bool(Js.Unsafe.get(el, "isConnected"))) {
  | _ => false
  };

/* is the caret mid-glide? the glide (a WAAPI animation) displaces its
   rect, which would poison the anchor. the permanent CSS blink is
   ignored: CSS animations have animationName/transitionProperty,
   WAAPI ones don't */
let animating = (el: Js.t(Dom_html.element)): bool =>
  try({
    let anims = Js.Unsafe.meth_call(el, "getAnimations", [||]);
    let n: int = Js.Unsafe.get(anims, "length");
    let rec go = (i: int): bool =>
      if (i >= n) {
        false;
      } else {
        let a = Js.Unsafe.get(anims, i);
        let is_css =
          Js.Optdef.test(Js.Unsafe.get(a, "animationName"))
          || Js.Optdef.test(Js.Unsafe.get(a, "transitionProperty"));
        is_css ? go(i + 1) : true;
      };
    go(0);
  }) {
  | _ => false
  };

let set_scroll_top = (g: geom, v: float): unit => {
  g.container##.scrollTop := int_of_float(v);
  /* browser clamps; keep the mirror exact */
  g.scroll_top = float_of_int(g.container##.scrollTop);
};

/* one stable handler reading the current geom, on at most one
   container: a closure per cold anchor would leak a handler (and its
   dead geom) per post-pause action */
let scroll_handler: Js.Unsafe.any =
  Js.Unsafe.inject(
    Js.wrap_callback(_ =>
      switch (geom^) {
      | Some(g) => g.scroll_top = float_of_int(g.container##.scrollTop)
      | None => ()
      }
    ),
  );
let listened: ref(option(Js.t(Dom_html.element))) = ref(None);
let ensure_scroll_listener = (container: Js.t(Dom_html.element)): unit => {
  let already =
    switch (listened^) {
    | Some(c) => c === container
    | None => false
    };
  if (!already) {
    switch (listened^) {
    | Some(old) =>
      let _ =
        Js.Unsafe.meth_call(
          old,
          "removeEventListener",
          [|Js.Unsafe.inject(Js.string("scroll")), scroll_handler|],
        );
      ();
    | None => ()
    };
    let _ =
      Js.Unsafe.meth_call(
        container,
        "addEventListener",
        [|Js.Unsafe.inject(Js.string("scroll")), scroll_handler|],
      );
    listened := Some(container);
  };
};

/* scroll delta keeping the caret's row out of the margin band, else 0 */
let decide = (g: geom, row: int, rh: float): float => {
  let y_top = g.editor_top +. float_of_int(row) *. rh -. g.scroll_top;
  let y_bot = y_top +. rh;
  let margin = g.height *. margin_ratio;
  if (y_top < margin) {
    y_top -. margin;
  } else if (y_bot > g.height -. margin) {
    y_bot -. (g.height -. margin);
  } else {
    0.;
  };
};

let apply = (g: geom, delta: float): unit =>
  if (delta != 0.) {
    set_scroll_top(g, g.scroll_top +. delta);
  };

let schedule_verify = (): unit =>
  if (! verify_scheduled^ && now_ms() -. last_verify_ms^ >= verify_min_gap_ms) {
    verify_scheduled := true;
    let _ =
      Dom_html.window##requestAnimationFrame(
        Js.wrap_callback((_: float) => {
          verify_scheduled := false;
          last_verify_ms := now_ms();
          switch (geom^, published^, JsUtil.get_elem_by_id_opt("caret")) {
          | (Some(g), Some((row, rh)), Some(caret))
              when
                caret === g.caret_el
                && connected(g.container)
                && !animating(caret) =>
            /* reading here costs the frame's own layout, not an
               extra mid-task flush */
            let caret_r = caret##getBoundingClientRect;
            let cont_r = g.container##getBoundingClientRect;
            g.height = Js.Optdef.get(cont_r##.height, _ => g.height);
            g.scroll_top = float_of_int(g.container##.scrollTop);
            let fresh =
              caret_r##.top
              -.
              cont_r##.top
              +. g.scroll_top
              -. float_of_int(row)
              *. rh;
            if (abs_float(fresh -. g.editor_top) > heal_tolerance_px) {
              g.editor_top = fresh;
              apply(g, decide(g, row, rh));
            };
          | _ => ()
          };
        }),
      );
    ();
  };

/* ground-truth reveal + anchor, in this frame's rAF: cold reveals
   follow a pause, so no keystroke races it, and rAF runs before paint,
   so the read costs the frame's own layout, not a mid-task flush */
let cold_scheduled = ref(false);
let schedule_cold = (): unit =>
  if (! cold_scheduled^) {
    cold_scheduled := true;
    let _ =
      Dom_html.window##requestAnimationFrame(
        Js.wrap_callback((_: float) => {
          cold_scheduled := false;
          switch (published^, JsUtil.get_elem_by_id_opt("caret")) {
          | (None, _)
          | (_, None) => ()
          | (Some((row, rh)), Some(caret)) =>
            switch (JsUtil.find_scroll_container_cached(caret)) {
            | None =>
              caret##scrollIntoView(
                Js.Unsafe.obj([|
                  ("block", Js.Unsafe.inject(Js.string("nearest"))),
                  ("inline", Js.Unsafe.inject(Js.string("nearest"))),
                |]),
              )
            | Some(container) =>
              let caret_r = caret##getBoundingClientRect;
              let cont_r = container##getBoundingClientRect;
              let height = Js.Optdef.get(cont_r##.height, _ => 0.);
              let margin = height *. margin_ratio;
              let scroll_pre = float_of_int(container##.scrollTop);
              let top_gap = caret_r##.top -. (cont_r##.top +. margin);
              let bottom_gap = caret_r##.bottom -. (cont_r##.bottom -. margin);
              let delta =
                if (top_gap < 0.) {
                  top_gap;
                } else if (bottom_gap > 0.) {
                  bottom_gap;
                } else {
                  0.;
                };
              JsUtil.adjust_scroll(container, delta);
              if (animating(caret)) {
                /* a mid-glide rect is off by at most the glide; don't
                   anchor on it, a later reveal will */
                geom := None;
              } else {
                let g = {
                  container,
                  caret_el: caret,
                  editor_top:
                    caret_r##.top
                    -.
                    cont_r##.top
                    +. scroll_pre
                    -. float_of_int(row)
                    *. rh,
                  height,
                  scroll_top: float_of_int(container##.scrollTop),
                };
                ensure_scroll_listener(container);
                geom := Some(g);
              };
            }
          };
        }),
      );
    ();
  };

let hooks_registered = ref(false);
let register_hooks = (): unit =>
  if (! hooks_registered^) {
    hooks_registered := true;
    /* geometry changes wholesale on resize; re-anchor */
    let on_resize = Js.wrap_callback(_ => geom := None);
    let _ =
      Js.Unsafe.meth_call(
        Dom_html.window,
        "addEventListener",
        [|
          Js.Unsafe.inject(Js.string("resize")),
          Js.Unsafe.inject(on_resize),
        |],
      );
    ();
  };

let reveal = (): unit => {
  register_hooks();
  let now = now_ms();
  let burst = now -. last_reveal_ms^ < burst_ms;
  last_reveal_ms := now;
  switch (published^) {
  | None => JsUtil.scroll_cursor_into_view_if_needed()
  | Some((row, rh)) =>
    /* getElementById is a lookup, not a layout read */
    let caret_now = JsUtil.get_elem_by_id_opt("caret");
    switch (geom^, caret_now) {
    | (Some(g), Some(caret))
        when burst && caret === g.caret_el && connected(g.container) =>
      /* synchronous on purpose: under long-task holds the rAF can
         lag behind keystrokes; the write keeps the caret pinned */
      apply(g, decide(g, row, rh));
      schedule_verify();
    | _ => schedule_cold()
    };
  };
};
