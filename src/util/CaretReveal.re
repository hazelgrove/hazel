/* Caret reveal without forced layout on the typing path (perf-ledger
   §17; modeled on Monaco's model-computed reveal and CodeMirror 6's
   batched measure phase). The caret's position within its editor is
   MODEL data — CaretDec positions it from Zipper.Caret.point × font
   metrics, and publishes (row, row_height) here at render time. The
   only DOM-dependent quantities are the editor's origin inside its
   scroll container and the scroll state. Those are ANCHORED by one
   real read on any reveal after a ≳500ms pause (clicks, jumps —
   exactly when the geometry may have changed) and kept as mirrors;
   reveals within a burst (held keys, typing) are pure arithmetic
   plus a scrollTop WRITE — no layout read. A throttled rAF
   verification re-reads ground truth against the frame's own layout
   (≈free there) and heals drift, e.g. stacked-mode cells above the
   caret growing mid-burst. window.__scrollCounters() keeps the
   regimes observable so quiet regressions stay visible. */

open Js_of_ocaml;

let margin_ratio = 0.10; /* trigger band, fraction of viewport height
                            (matches scroll_vertically_into_view) */

let burst_ms = 500.;
let verify_min_gap_ms = 150.;
let heal_tolerance_px = 2.;

type geom = {
  container: Js.t(Dom_html.element),
  /* content-space y of the active editor's row 0 */
  mutable editor_top: float,
  mutable height: float,
  /* mirror of container##.scrollTop: our writes + a scroll listener
     (fires for programmatic writes too, e.g. jump top-align) */
  mutable scroll_top: float,
};

let published: ref(option((int, float))) = ref(None);
let publish = (~row: int, ~row_height: float): unit =>
  published := Some((row, row_height));

let geom: ref(option(geom)) = ref(None);
let last_reveal_ms: ref(float) = ref(0.);
let last_verify_ms: ref(float) = ref(0.);
let verify_scheduled = ref(false);

let n_cold = ref(0);
let n_arith = ref(0);
let n_arith_scrolled = ref(0);
let n_verified = ref(0);
let n_healed = ref(0);
let n_fallback = ref(0);

let now_ms = (): float => Js.Unsafe.global##.Date##now();

let connected = (el: Js.t(Dom_html.element)): bool =>
  try(Js.to_bool(Js.Unsafe.get(el, "isConnected"))) {
  | _ => false
  };

/* the caret glide (Animation.Actions.move, a Web-Animations-API
   `animate` call) transforms the caret; rects read mid-glide are
   displaced and would poison the anchor. The caret ALSO carries a
   permanent CSS blink — CSS animations/transitions have an
   animationName/transitionProperty, WAAPI ones don't, which is how
   we ignore the blink (found via the cold/arith counters: the
   blanket getAnimations check pinned every reveal to the cold
   path). */
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

let attach_scroll_listener = (g: geom): unit => {
  let handler =
    Js.wrap_callback(_ =>
      g.scroll_top = float_of_int(g.container##.scrollTop)
    );
  let _ =
    Js.Unsafe.meth_call(
      g.container,
      "addEventListener",
      [|Js.Unsafe.inject(Js.string("scroll")), Js.Unsafe.inject(handler)|],
    );
  ();
};

/* the reveal decision from mirrored geometry: scroll delta to keep
   the caret's row-box outside the margin band, 0 when safe */
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
          incr(n_verified);
          switch (geom^, published^, JsUtil.get_elem_by_id_opt("caret")) {
          | (Some(g), Some((row, rh)), Some(caret))
              when connected(g.container) && !animating(caret) =>
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
              incr(n_healed);
              g.editor_top = fresh;
              apply(g, decide(g, row, rh));
            };
          | _ => ()
          };
        }),
      );
    ();
  };

/* ground-truth reveal + anchor. Deferred to the SAME frame's rAF:
   cold reveals follow a pause, so no next keystroke races the rAF
   (the failure mode of blanket deferral), and rAF runs before paint,
   so the scroll is never visibly late — the read then costs the
   frame's own layout instead of a mid-task flush of the freshly
   patched tree. One rect pair serves both the reveal decision and
   the burst anchor. */
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
              incr(n_fallback);
              caret##scrollIntoView(
                Js.Unsafe.obj([|
                  ("block", Js.Unsafe.inject(Js.string("nearest"))),
                  ("inline", Js.Unsafe.inject(Js.string("nearest"))),
                |]),
              );
            | Some(container) =>
              incr(n_cold);
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
                /* mid-glide rect (caret FLIP transform): reveal from
                   it is transiently off by ≤ the glide distance and
                   self-corrects; don't poison the anchor — glides
                   are rate-gated, a following reveal anchors */
                geom := None;
              } else {
                let g = {
                  container,
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
                attach_scroll_listener(g);
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
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__scrollCounters",
      Js.wrap_callback(() =>
        Js.Unsafe.obj([|
          ("cold", Js.Unsafe.inject(n_cold^)),
          ("arith", Js.Unsafe.inject(n_arith^)),
          ("arithScrolled", Js.Unsafe.inject(n_arith_scrolled^)),
          ("verified", Js.Unsafe.inject(n_verified^)),
          ("healed", Js.Unsafe.inject(n_healed^)),
          ("fallback", Js.Unsafe.inject(n_fallback^)),
        |])
      ),
    );
  };

let reveal = (): unit => {
  register_hooks();
  let now = now_ms();
  let burst = now -. last_reveal_ms^ < burst_ms;
  last_reveal_ms := now;
  switch (published^) {
  | None => JsUtil.scroll_cursor_into_view_if_needed()
  | Some((row, rh)) =>
    switch (geom^) {
    | Some(g) when burst && connected(g.container) =>
      /* synchronous on purpose: under long-task holds the rAF can
         lag behind keystrokes; the write keeps the caret pinned */
      incr(n_arith);
      let delta = decide(g, row, rh);
      if (delta != 0.) {
        incr(n_arith_scrolled);
        apply(g, delta);
      };
      schedule_verify();
    | _ => schedule_cold()
    }
  };
};
