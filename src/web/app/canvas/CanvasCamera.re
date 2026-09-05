/* CanvasCamera — the one writer of the canvas "camera": viewport center
   (board coords) + zoom; scroll is derived. fit and follow are both camera
   animations, so they can never fight over scrollLeft/Top.

   Follow rule (canvas_follow): the avatar roams freely inside a central
   dead zone; a hop whose destination lands outside it glides the camera
   so the destination ends up central-ish, in the same 450 ms as the
   avatar's own hop (destination is known at beat time — choreography, not
   lag-follow). The user's own scrolling is never fought: the camera only
   moves on off-zone hops, and a manual wheel zoom pins the zoom for a
   while. Zoom follows a region of interest (sites touched this burst)
   with hysteresis, replacing the old zoom-out-only auto-fit. */

open Js_of_ocaml;

let pan_slack = CanvasRipple.pan_slack;
let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let zoom_now: ref(float) = ref(1.);
/* set each render by CanvasSidebar: commits a zoom into settings */
let zoom_send: ref(option(float => unit)) =
  ref(None: option(float => unit));

/* a newer animation supersedes a running one */
let gen: ref(int) = ref(0);
/* the glide in flight (target center, target zoom): a request for the
   same place while it runs is a no-op instead of a restart */
let inflight: ref(option(((float, float), float))) =
  ref(None: option(((float, float), float)));
let same_target = ((cx, cy): (float, float), z: float): bool =>
  switch (inflight^) {
  | Some(((ix, iy), iz)) =>
    abs_float(ix -. cx) < 2.
    && abs_float(iy -. cy) < 2.
    && abs_float(iz -. z) < 0.01
  | None => false
  };
/* while we write scroll (plus a grace tail) scroll events are ours */
let driving_until: ref(float) = ref(0.);
let mark_driving = (): unit => driving_until := now() +. 220.;
/* a manual wheel zoom pins the zoom against ROI adjustments for a while */
let user_zoom_until: ref(float) = ref(0.);
/* while a score drives the camera, the generic follow stands down */
let scored_until: ref(float) = ref(0.);

/* ---- exposure (C2): how long each node has been in frame. New until
   it has been seen for exposed_after_ms; only then may the camera let
   it go. ---- */
let exposure: ref(list((string, float))) = ref([]);
let exposed_after_ms = 2000.;
let last_exposure_tick: ref(float) = ref(0.);
let exposed_keys = (): list(string) =>
  List.filter_map(
    ((k, ms)) => ms >= exposed_after_ms ? Some(k) : None,
    exposure^,
  );
let reset_exposure = (): unit => {
  exposure := [];
  last_exposure_tick := 0.;
};
let note_user_zoom = (): unit => user_zoom_until := now() +. 10000.;
/* user scrolled since the last glide (informational; the dead-zone rule
   already lets manual panning win between hops) */
let user_scrolled: ref(bool) = ref(false);
let note_scroll = (): unit =>
  if (now() > driving_until^) {
    user_scrolled := true;
  };

let scroll_el = () => Util.JsUtil.get_elem_by_id_opt("canvas-scroll");
let root_el = (): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(
    Js.Unsafe.meth_call(
      Js.Unsafe.global##.document,
      "querySelector",
      [|Js.Unsafe.inject(Js.string(".canvas-root"))|],
    ),
  );

/* viewport center in board coords, read from the DOM (user scrolls are
   the truth) */
let center = (~aw: float, ~ah: float): option((float, float)) =>
  switch (scroll_el()) {
  | None => None
  | Some(el) =>
    let el = Js.Unsafe.coerce(el);
    let sl: float = el##.scrollLeft
    and st: float = el##.scrollTop;
    let z = zoom_now^;
    Some((
      (sl -. pan_slack +. aw /. 2.) /. z,
      (st -. pan_slack +. ah /. 2.) /. z,
    ));
  };

let apply =
    (~aw: float, ~ah: float, (cx, cy): (float, float), z: float): unit => {
  switch (root_el()) {
  | Some(root) =>
    Js.Unsafe.coerce(root)##.style##.zoom :=
      Js.string(Printf.sprintf("%.4f", z))
  | None => ()
  };
  zoom_now := z;
  CanvasRipple.zoom := z;
  switch (scroll_el()) {
  | Some(el) =>
    let el = Js.Unsafe.coerce(el);
    el##.scrollLeft := max(0., pan_slack +. cx *. z -. aw /. 2.);
    el##.scrollTop := max(0., pan_slack +. cy *. z -. ah /. 2.);
  | None => ()
  };
  mark_driving();
  CanvasRipple.request_draw();
};

type easing =
  | EaseOut /* fit: decelerate into place */
  | EaseInOut; /* follow: matches the avatar's symmetric hop */

let ease = (e: easing, t: float): float =>
  switch (e) {
  | EaseOut => 1. -. (1. -. t) ** 3.
  | EaseInOut =>
    t < 0.5 ? 4. *. t *. t *. t : 1. -. ((-2.) *. t +. 2.) ** 3. /. 2.
  };

let class_toggle = (cls: string, on: bool): unit =>
  switch (root_el()) {
  | Some(root) =>
    let cl = Js.Unsafe.get(Js.Unsafe.coerce(root), "classList");
    ignore(
      Js.Unsafe.meth_call(
        cl,
        on ? "add" : "remove",
        [|Js.Unsafe.inject(Js.string(cls))|],
      ),
    );
  | None => ()
  };

/* glide the camera to a center (and optionally a zoom) over dur ms;
   scroll and zoom move together so the target stays pinned */
let animate =
    (
      ~aw: float,
      ~ah: float,
      ~zoom: option(float)=None,
      ~dur: float=450.,
      ~easing: easing=EaseInOut,
      (cx1, cy1): (float, float),
    )
    : unit =>
  switch (center(~aw, ~ah)) {
  | None => ()
  | Some((cx0, cy0)) =>
    incr(gen);
    let g = gen^;
    let z0 = zoom_now^;
    let z1 = Option.value(~default=z0, zoom);
    inflight := Some(((cx1, cy1), z1));
    /* the CSS zoom transition would fight the per-frame writes */
    class_toggle("no-zoom-anim", true);
    let t0: ref(float) = ref(0.);
    let rec step = (t_now: float) =>
      if (g == gen^) {
        if (t0^ == 0.) {
          t0 := t_now;
        };
        let t = min(1., (t_now -. t0^) /. dur);
        let e = ease(easing, t);
        apply(
          ~aw,
          ~ah,
          (cx0 +. (cx1 -. cx0) *. e, cy0 +. (cy1 -. cy0) *. e),
          z0 +. (z1 -. z0) *. e,
        );
        if (t < 1.) {
          ignore(
            Js.Unsafe.meth_call(
              Js.Unsafe.global##.window,
              "requestAnimationFrame",
              [|Js.Unsafe.inject(Js.Unsafe.callback(step))|],
            ),
          );
        } else {
          class_toggle("no-zoom-anim", false);
          if (same_target((cx1, cy1), z1)) {
            inflight := None;
          };
          if (z1 != z0) {
            switch (zoom_send^) {
            | Some(send) => send(z1)
            | None => ()
            };
          };
        };
      };
    ignore(
      Js.Unsafe.meth_call(
        Js.Unsafe.global##.window,
        "requestAnimationFrame",
        [|Js.Unsafe.inject(Js.Unsafe.callback(step))|],
      ),
    );
  };

/* ---- follow ---- */

/* dead zone: the central box (this fraction of the pane per axis) the
   avatar may roam without moving the camera */
let dead_zone_frac = 0.6;
/* a glide pulls the destination this far toward center (not all the way:
   hysteresis, and it reads as "dragging along" rather than snapping) */
let recenter_pull = 0.7;
let zoom_min = 0.7;
let zoom_max = 1.3;
/* board-px padding around the region of interest when fitting it */
let roi_pad = 110.;

/* the graph's extent in board coords, set each render by CanvasSidebar:
   when the whole program fits the pane at a legible zoom, follow keeps
   ALL of it in view rather than centering the avatar in empty canvas */
let graph_bbox: ref(option((float, float, float, float))) =
  ref(None: option((float, float, float, float)));
let fit_pad = 12.;
let fits_whole = (~aw, ~ah): option(((float, float), float)) =>
  switch (graph_bbox^) {
  | None => None
  | Some((x0, y0, x1, y1)) =>
    let gw = max(1., x1 -. x0)
    and gh = max(1., y1 -. y0);
    /* the layout itself fills the pane, so framing pads only a sliver */
    let fit = min((aw -. 2. *. fit_pad) /. gw, (ah -. 2. *. fit_pad) /. gh);
    /* framing only ever zooms OUT to fit: a small program sits at 1:1 so
       the viewport holds still while it grows (layout fills the pane) */
    fit >= zoom_min
      ? Some((((x0 +. x1) /. 2., (y0 +. y1) /. 2.), min(1., fit))) : None;
  };
/* is the whole graph already inside the viewport (with a margin)? */
let whole_visible = (~aw, ~ah): bool =>
  switch (graph_bbox^, center(~aw, ~ah)) {
  | (Some((x0, y0, x1, y1)), Some((cx, cy))) =>
    let z = zoom_now^;
    let hw = aw /. 2. /. z
    and hh = ah /. 2. /. z;
    x0 >= cx
    -. hw
    +. 4.
    && x1 <= cx
    +. hw
    -. 4.
    && y0 >= cy
    -. hh
    +. 4.
    && y1 <= cy
    +. hh
    -. 4.;
  | _ => false
  };

/* sites touched during the current burst (dest points); reset when the
   burst window lapses */
let roi: ref(list((float, float))) = ref([]);
let roi_last_touch: ref(float) = ref(0.);
let touch = ((x, y): (float, float)): unit => {
  if (now() -. roi_last_touch^ > Util.AgentPulse.burst_window_ms) {
    roi := [];
  };
  roi_last_touch := now();
  if (!List.mem((x, y), roi^)) {
    roi := [(x, y), ...roi^];
  };
};

let in_dead_zone = (~aw, ~ah, (px, py): (float, float)): bool =>
  switch (
    switch (inflight^) {
    | Some((c, _)) => Some(c) /* judge against where the camera is heading */
    | None => center(~aw, ~ah)
    }
  ) {
  | None => true
  | Some((cx, cy)) =>
    let z = zoom_now^;
    abs_float(px -. cx) < dead_zone_frac
    *. aw
    /. 2.
    /. z
    && abs_float(py -. cy) < dead_zone_frac
    *. ah
    /. 2.
    /. z;
  };

/* zoom the ROI asks for, with hysteresis: only when it doesn't fit at the
   current zoom, or is tiny; None = keep */
let roi_zoom = (~aw, ~ah): option(float) =>
  if (now() < user_zoom_until^ || List.length(roi^) < 2) {
    None;
  } else {
    let xs = List.map(fst, roi^)
    and ys = List.map(snd, roi^);
    let bw = List.fold_left(max, -1e9, xs) -. List.fold_left(min, 1e9, xs)
    and bh = List.fold_left(max, -1e9, ys) -. List.fold_left(min, 1e9, ys);
    let fit =
      min(aw /. (bw +. 2. *. roi_pad), ah /. (bh +. 2. *. roi_pad))
      |> max(zoom_min)
      |> min(zoom_max);
    let z = zoom_now^;
    if (fit < z -. 0.05) {
      Some
        (fit); /* doesn't fit: zoom out to it */
    } else if (z < zoom_max && fit > z *. 1.5) {
      Some
        (min(zoom_max, z *. 1.25)); /* tiny: ease back in, gently */
    } else {
      None;
    };
  };

let roi_center = (): option((float, float)) =>
  switch (roi^) {
  | [] => None
  | pts =>
    let n = float_of_int(List.length(pts));
    Some((
      List.fold_left((a, (x, _)) => a +. x, 0., pts) /. n,
      List.fold_left((a, (_, y)) => a +. y, 0., pts) /. n,
    ));
  };

/* the avatar hopped to (or started working at) dest */
let rec follow = (~aw: float, ~ah: float, dest: (float, float)): unit => {
  touch(dest);
  switch (fits_whole(~aw, ~ah)) {
  | Some((gc, zfit)) =>
    /* small program: keep all of it in view. Re-frame only when part of
       it is off-screen or the zoom is clearly off (hysteresis); a manual
       wheel zoom pins the zoom */
    let z = zoom_now^;
    let want_zoom =
      now() < user_zoom_until^ || abs_float(zfit -. z) < 0.08
        ? None : Some(zfit);
    let target_z = Option.value(~default=z, want_zoom);
    if ((!whole_visible(~aw, ~ah) || want_zoom != None)
        && !same_target(gc, target_z)) {
      CanvasLog.log(
        Printf.sprintf(
          "camera: frame whole program -> (%.0f, %.0f)%s",
          fst(gc),
          snd(gc),
          switch (want_zoom) {
          | Some(zz) => Printf.sprintf(" zoom %.2f -> %.2f", z, zz)
          | None => ""
          },
        ),
      );
      animate(~aw, ~ah, ~zoom=want_zoom, ~dur=450., ~easing=EaseInOut, gc);
    };
  | None => follow_site(~aw, ~ah, dest)
  };
}
/* the program outgrows the pane: follow the site with the dead zone */
and follow_site = (~aw: float, ~ah: float, dest: (float, float)): unit => {
  let zoom = roi_zoom(~aw, ~ah);
  switch (center(~aw, ~ah), in_dead_zone(~aw, ~ah, dest), zoom) {
  | (None, _, _) => ()
  | (Some(_), true, None) => () /* roaming inside the zone: hold still */
  | (Some((cx, cy)), inside, zoom) =>
    let (px, py) = dest;
    let target =
      switch (zoom, roi_center()) {
      | (Some(_), Some(rc)) when !inside => rc /* refit: frame the ROI */
      | (Some(_), Some(rc)) => rc
      | _ => (
          cx +. (px -. cx) *. recenter_pull,
          cy +. (py -. cy) *. recenter_pull,
        )
      };
    user_scrolled := false;
    CanvasLog.log(
      Printf.sprintf(
        "camera: glide -> (%.0f, %.0f)%s",
        fst(target),
        snd(target),
        switch (zoom) {
        | Some(z) => Printf.sprintf(" zoom %.2f -> %.2f", zoom_now^, z)
        | None => ""
        },
      ),
    );
    animate(~aw, ~ah, ~zoom, ~dur=450., ~easing=EaseInOut, target);
  };
};

/* console testers: __canvasCameraTo(x, y[, z]) and __canvasFollowTo(x, y)
   drive the camera against the live pane (pane size from the DOM) */
/* called per render with the nodes' board positions; credits the time
   since the last render (capped) to every node inside the viewport */
let note_exposure =
    (~aw: float, ~ah: float, nodes: list((string, (float, float)))): unit =>
  switch (center(~aw, ~ah)) {
  | None => ()
  | Some((cx, cy)) =>
    let t = now();
    let dt =
      last_exposure_tick^ == 0. ? 0. : min(500., t -. last_exposure_tick^);
    last_exposure_tick := t;
    let z = zoom_now^;
    let hw = aw /. 2. /. z
    and hh = ah /. 2. /. z;
    exposure :=
      List.map(
        ((k, (x, y))) => {
          let inside =
            x >= cx -. hw && x <= cx +. hw && y >= cy -. hh && y <= cy +. hh;
          let prev = Option.value(~default=0., List.assoc_opt(k, exposure^));
          (k, inside ? prev +. dt : prev);
        },
        nodes,
      );
  };

let pane = (): option((float, float)) =>
  switch (scroll_el()) {
  | Some(el) =>
    let el = Js.Unsafe.coerce(el);
    Some((float_of_int(el##.clientWidth), float_of_int(el##.clientHeight)));
  | None => None
  };
let install_testers = (): unit => {
  let g = Js.Unsafe.global;
  if (!Js.Optdef.test(Js.Unsafe.get(g, "__canvasCameraTo"))) {
    Js.Unsafe.set(
      g,
      "__canvasCameraTo",
      Js.Unsafe.callback((x: float, y: float, z: Js.Optdef.t(float)) =>
        switch (pane()) {
        | Some((aw, ah)) =>
          animate(
            ~aw,
            ~ah,
            ~zoom=Js.Optdef.to_option(z),
            ~easing=EaseOut,
            (x, y),
          )
        | None => ()
        }
      ),
    );
    Js.Unsafe.set(
      g,
      "__canvasFollowTo",
      Js.Unsafe.callback((x: float, y: float) =>
        switch (pane()) {
        | Some((aw, ah)) => follow(~aw, ~ah, (x, y))
        | None => ()
        }
      ),
    );
  };
};
