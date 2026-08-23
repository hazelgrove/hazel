open Js_of_ocaml;

/* The dot lattice, promoted from a repeated CSS background to a 2D
   canvas so dots can MOVE: placing a node splashes a compression wave
   through the grid — dots displace radially under a traveling gaussian
   ring that decays as it expands.

   The element keeps the CSS mask (edge fade) and rides the root's CSS
   zoom; pitch and dot radius are read from the root's inline
   --dot-pitch/--dot-r so zoom-looping behavior is unchanged. Static
   redraws happen only when geometry changes; an rAF loop runs only
   while ripples are live. */

/* ---- the wave medium ----
   A damped 2D wave field simulated at ~half dot resolution over the
   viewport, replacing the old superposed analytic gaussian rings.
   Every disturbance (splash, suction, the drag wake, rainfall) is an
   energy injection into ONE medium, so interference and reflections
   emerge instead of being composed — and dragging a node sheds waves
   along its path like a stick pulled through water (with the wave
   speed tuned below typical drag speed, a Mach-cone wake forms
   physically). Dots displace by the local field gradient: an O(1)
   lookup per dot, cheaper than the old per-ripple loop.

   The field is pinned to CONTENT coordinates (cells shift on scroll)
   so waves stay where they happened while panning. */

/* view state, pushed in by CanvasSidebar each render (and imperatively
   during a pinch): the dots canvas is VIEWPORT-fixed and needs the
   board->screen mapping */
let zoom: ref(float) = ref(1.);
let pan_slack = 392.; /* keep in sync with CanvasSidebar.pan_slack */

let cell = 7.; /* field resolution, screen px */
let stiffness = 0.35; /* (c*dt/cell)^2 — CFL-stable below 0.5 */
let damping = 0.988; /* per substep */
let grad_gain = 5.5; /* field gradient -> dot displacement px */
let default_amp = 9.;
let stroke_amp = 3.2; /* drag-wake deposit per sample point */

let gw: ref(int) = ref(0);
let gh: ref(int) = ref(0);
let u_cur: ref(array(float)) = ref([||]);
let u_prev: ref(array(float)) = ref([||]);
/* content-px position of cell (0,0) */
let origin_x: ref(float) = ref(0.);
let origin_y: ref(float) = ref(0.);
let sim_active: ref(bool) = ref(false);
let last_step: ref(float) = ref(0.);

let zoom_ref = zoom; /* alias for clarity below */

let idx = (i: int, j: int): int => j * gw^ + i;

let ensure_grid = (cw: int, ch: int): unit => {
  let w = int_of_float(Float.ceil(float_of_int(cw) /. cell)) + 3;
  let h = int_of_float(Float.ceil(float_of_int(ch) /. cell)) + 3;
  if (w != gw^ || h != gh^) {
    gw := w;
    gh := h;
    u_cur := Array.make(w * h, 0.);
    u_prev := Array.make(w * h, 0.);
  };
};

/* keep the field pinned to content while the viewport scrolls: shift
   cells by whole-cell deltas */
let anchor = (sl: float, st: float): unit => {
  let target_x = sl -. cell
  and target_y = st -. cell;
  let dx = int_of_float(Float.round((target_x -. origin_x^) /. cell))
  and dy = int_of_float(Float.round((target_y -. origin_y^) /. cell));
  if (dx != 0 || dy != 0) {
    let w = gw^
    and h = gh^;
    let shift = (a: array(float)): array(float) => {
      let b = Array.make(w * h, 0.);
      for (j in 0 to h - 1) {
        for (i in 0 to w - 1) {
          let si = i + dx
          and sj = j + dy;
          if (si >= 0 && si < w && sj >= 0 && sj < h) {
            b[j * w + i] = a[sj * w + si];
          };
        };
      };
      b;
    };
    u_cur := shift(u_cur^);
    u_prev := shift(u_prev^);
    origin_x := origin_x^ +. float_of_int(dx) *. cell;
    origin_y := origin_y^ +. float_of_int(dy) *. cell;
  };
};

/* deposit an impulse (3x3 kernel) at a CONTENT-px point */
let deposit = (cx: float, cy: float, amp: float): unit =>
  if (gw^ > 0) {
    let ci = int_of_float(Float.round((cx -. origin_x^) /. cell))
    and cj = int_of_float(Float.round((cy -. origin_y^) /. cell));
    for (j in cj - 1 to cj + 1) {
      for (i in ci - 1 to ci + 1) {
        if (i >= 0 && i < gw^ && j >= 0 && j < gh^) {
          let k = i == ci && j == cj ? 1.0 : 0.35;
          u_cur^[idx(i, j)] = u_cur^[idx(i, j)] +. amp *. k;
        };
      };
    };
    sim_active := true;
  };

let model_to_content = ((x, y): (float, float)): (float, float) => (
  pan_slack +. x *. zoom_ref^,
  pan_slack +. y *. zoom_ref^,
);

let step_sim = (substeps: int): unit => {
  let w = gw^
  and h = gh^;
  if (w > 0) {
    for (_ in 1 to substeps) {
      let u = u_cur^
      and up = u_prev^;
      let un = Array.make(w * h, 0.);
      for (j in 1 to h - 2) {
        for (i in 1 to w - 2) {
          let k = j * w + i;
          let lap = u[k - 1] +. u[k + 1] +. u[k - w] +. u[k + w] -. 4. *. u[k];
          un[k] = damping *. (2. *. u[k] -. up[k] +. stiffness *. lap);
        };
      };
      u_prev := u;
      u_cur := un;
    };
    /* cheap liveness probe: sample every 5th cell */
    let e = ref(0.);
    let u = u_cur^;
    let n = Array.length(u);
    let i = ref(0);
    while (i^ < n) {
      e := e^ +. abs_float(u[i^]);
      i := i^ + 5;
    };
    sim_active := e^ > 0.4;
  };
};

/* field gradient at a content-px point -> dot displacement */
let displacement_at = (cx: float, cy: float): (float, float) =>
  if (gw^ == 0) {
    (0., 0.);
  } else {
    let i = int_of_float(Float.round((cx -. origin_x^) /. cell))
    and j = int_of_float(Float.round((cy -. origin_y^) /. cell));
    if (i < 1 || i >= gw^ - 1 || j < 1 || j >= gh^ - 1) {
      (0., 0.);
    } else {
      let u = u_cur^;
      let gx = (u[idx(i + 1, j)] -. u[idx(i - 1, j)]) /. 2.
      and gy = (u[idx(i, j + 1)] -. u[idx(i, j - 1)]) /. 2.;
      (grad_gain *. gx, grad_gain *. gy);
    };
  };

/* drag wake: injection along the path between successive drag samples */
let field: ref(option((float, float))) =
  ref(None: option((float, float)));

let raf_running: ref(bool) = ref(false);
let draw_queued: ref(bool) = ref(false);
/* geometry of the last static draw, to skip redundant repaints */
let last_geom: ref((int, int, float, float, float)) =
  ref((0, 0, 0., 0., 0.));

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let parse_px = (s: string): option(float) => {
  let s = String.trim(s);
  let s =
    String.length(s) > 2 && String.sub(s, String.length(s) - 2, 2) == "px"
      ? String.sub(s, 0, String.length(s) - 2) : s;
  float_of_string_opt(s);
};

let css_var = (el: Js.Unsafe.any, name: string): option(float) => {
  let v: Js.t(Js.js_string) =
    Js.Unsafe.meth_call(
      Js.Unsafe.coerce(el)##.style,
      "getPropertyValue",
      [|Js.Unsafe.inject(Js.string(name))|],
    );
  parse_px(Js.to_string(v));
};

let rec draw = (): unit => {
  draw_queued := false;
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-dots")) {
  | None => ()
  | Some(el) =>
    let el = Js.Unsafe.coerce(el);
    /* holder -> #canvas-scroll */
    let scroll = Js.Unsafe.coerce(el##.parentElement)##.parentElement;
    let t = now();
    let dpr: float = Js.Unsafe.coerce(Js.Unsafe.global)##.devicePixelRatio;
    let cw: int = Js.Unsafe.coerce(scroll)##.clientWidth
    and ch: int = Js.Unsafe.coerce(scroll)##.clientHeight;
    let sl: float = Js.Unsafe.coerce(scroll)##.scrollLeft
    and st: float = Js.Unsafe.coerce(scroll)##.scrollTop;
    /* track the VISUAL zoom: during the auto-fit/fit zoom transition the
       computed style interpolates, and following it keeps the lattice
       coupled to the board instead of snapping ahead of it */
    let vis_zoom = {
      let root =
        Js.Opt.to_option(
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.document,
            "querySelector",
            [|Js.Unsafe.inject(Js.string(".canvas-root"))|],
          ),
        );
      switch (root) {
      | None => zoom^
      | Some(root) =>
        let v: Js.t(Js.js_string) =
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.window,
            "getComputedStyle",
            [|Js.Unsafe.inject(root)|],
          )##getPropertyValue(
            Js.string("zoom"),
          );
        switch (float_of_string_opt(String.trim(Js.to_string(v)))) {
        | Some(f) when f > 0.01 => f
        | _ => zoom^
        };
      };
    };
    let zoom_settling = abs_float(vis_zoom -. zoom^) > 0.002;
    let z = max(0.05, vis_zoom);
    let bw = int_of_float(float_of_int(cw) *. dpr)
    and bh = int_of_float(float_of_int(ch) *. dpr);
    if (el##.width != bw || el##.height != bh) {
      el##.width := bw;
      el##.height := bh;
      Js.Unsafe.coerce(el)##.style##.width :=
        Js.string(string_of_int(cw) ++ "px");
      Js.Unsafe.coerce(el)##.style##.height :=
        Js.string(string_of_int(ch) ++ "px");
      last_geom := ((-1), (-1), 0., 0., 0.);
    };
    ensure_grid(cw, ch);
    anchor(sl, st);
    /* advance the medium by wall-clock (capped substeps keep long
       frames stable) */
    if (sim_active^) {
      let dt_ms = 8.;
      let elapsed = last_step^ == 0. ? dt_ms : min(48., t -. last_step^);
      step_sim(max(1, int_of_float(elapsed /. dt_ms)));
    };
    last_step := t;
    let geom = (cw, ch, sl, st, z);
    if (sim_active^ || geom != last_geom^) {
      last_geom := geom;
      let ctx =
        Js.Unsafe.meth_call(
          el,
          "getContext",
          [|Js.Unsafe.inject(Js.string("2d"))|],
        );
      let mw = float_of_int(cw)
      and mh = float_of_int(ch);
      let _ =
        Js.Unsafe.meth_call(
          ctx,
          "setTransform",
          [|
            Js.Unsafe.inject(dpr),
            Js.Unsafe.inject(0.),
            Js.Unsafe.inject(0.),
            Js.Unsafe.inject(dpr),
            Js.Unsafe.inject(0.),
            Js.Unsafe.inject(0.),
          |],
        );
      let _ =
        Js.Unsafe.meth_call(
          ctx,
          "clearRect",
          [|
            Js.Unsafe.inject(0.),
            Js.Unsafe.inject(0.),
            Js.Unsafe.inject(mw),
            Js.Unsafe.inject(mh),
          |],
        );
      let fill: Js.t(Js.js_string) =
        Js.Unsafe.meth_call(
          Js.Unsafe.global##.window,
          "getComputedStyle",
          [|Js.Unsafe.inject(scroll)|],
        )##getPropertyValue(
          Js.string("--BR1"),
        );
      let fill = Js.to_string(fill);
      Js.Unsafe.coerce(ctx)##.fillStyle :=
        Js.string(fill == "" ? "#d8c9a3" : fill);
      let two_pi = 2. *. Float.pi;
      /* continuous level-of-detail: the "current" grid is the power-of-
         two multiple of the base 14px lattice whose SCREEN pitch lands
         in [14, 28); as zooming grows it past 28 the next finer grid
         (screen pitch [7, 14)) has already faded in underneath and
         takes over — a smooth crossfade instead of the old abrupt
         subdivision */
      let g = ref(14.); /* current grid step, model units */
      let p = ref(14. *. z); /* its screen pitch */
      while (p^ < 14.) {
        g := g^ *. 2.;
        p := p^ *. 2.;
      };
      while (p^ >= 28.) {
        g := g^ /. 2.;
        p := p^ /. 2.;
      };
      let f = (p^ -. 14.) /. 14.; /* 0..1 position between levels */
      let fine_alpha = f *. f; /* ease-in so the fine grid arrives late */
      /* dots fade approaching the PANEL edges (the viewport is the
         frame now, not the board: no more island-and-abyss) */
      let edge = 26.;
      let fade1 = (v: float, extent: float): float => {
        let d = min(v, extent -. v);
        d <= 0. ? 0. : min(1., d /. edge);
      };
      /* board model coord -> screen px */
      let to_screen_x = (m: float): float => m *. z +. pan_slack -. sl
      and to_screen_y = (m: float): float => m *. z +. pan_slack -. st;
      let draw_pass = (step: float, alpha: float, ~skip_coarse: bool) => {
        let m0x = (0. +. sl -. pan_slack) /. z
        and m1x = (mw +. sl -. pan_slack) /. z;
        let m0y = (0. +. st -. pan_slack) /. z
        and m1y = (mh +. st -. pan_slack) /. z;
        let i0 = int_of_float(Float.floor(m0x /. step))
        and i1 = int_of_float(Float.ceil(m1x /. step));
        let j0 = int_of_float(Float.floor(m0y /. step))
        and j1 = int_of_float(Float.ceil(m1y /. step));
        for (jy in j0 to j1) {
          for (ix in i0 to i1) {
            /* the fine pass skips points shared with the current grid
               (they are redrawn at full alpha anyway) */
            if (!(skip_coarse && ix mod 2 == 0 && jy mod 2 == 0)) {
              let x = float_of_int(ix) *. step
              and y = float_of_int(jy) *. step;
              /* content px of this dot, displaced by the wave field */
              let cx = x *. z +. pan_slack
              and cy = y *. z +. pan_slack;
              let (dx, dy) = displacement_at(cx, cy);
              let sx = cx +. dx -. sl
              and sy = cy +. dy -. st;
              let a = alpha *. fade1(sx, mw) *. fade1(sy, mh);
              if (a > 0.01) {
                Js.Unsafe.coerce(ctx)##.globalAlpha := a;
                let _ = Js.Unsafe.meth_call(ctx, "beginPath", [||]);
                let _ =
                  Js.Unsafe.meth_call(
                    ctx,
                    "arc",
                    [|
                      Js.Unsafe.inject(sx),
                      Js.Unsafe.inject(sy),
                      Js.Unsafe.inject(0.75),
                      Js.Unsafe.inject(0.),
                      Js.Unsafe.inject(two_pi),
                    |],
                  );
                let _ = Js.Unsafe.meth_call(ctx, "fill", [||]);
                ();
              };
            };
          };
        };
      };
      if (fine_alpha > 0.02) {
        draw_pass(g^ /. 2., fine_alpha, ~skip_coarse=true);
      };
      draw_pass(g^, 1., ~skip_coarse=false);
      Js.Unsafe.coerce(ctx)##.globalAlpha := 1.;
    };
    if (sim_active^ || zoom_settling) {
      if (! raf_running^) {
        raf_running := true;
      };
      let _ =
        Js.Unsafe.meth_call(
          Js.Unsafe.global##.window,
          "requestAnimationFrame",
          [|Js.Unsafe.inject(Js.Unsafe.callback(() => draw()))|],
        );
      ();
    } else {
      raf_running := false;
    };
  };
};

/* schedule one draw after the current render is painted (post-patch) */
let request_draw = (): unit =>
  if (! draw_queued^ && ! raf_running^) {
    draw_queued := true;
    let _ =
      Js.Unsafe.meth_call(
        Js.Unsafe.global##.window,
        "requestAnimationFrame",
        [|Js.Unsafe.inject(Js.Unsafe.callback(() => draw()))|],
      );
    ();
  };

/* splash: one impulse into the medium at a model-space point */
let splash = (~amp: float=default_amp, (x, y): (float, float)): unit => {
  let (cx, cy) = model_to_content((x, y));
  deposit(cx, cy, amp);
  request_draw();
};

/* drag wake: deposit energy along the path between successive drag
   samples — the stick pulled through water. Cleared on drop; the shed
   waves persist and ring down on their own. */
let set_field = (p: option((float, float))): unit => {
  switch (field^, p) {
  | (Some((px, py)), Some((x, y))) =>
    let (c0x, c0y) = model_to_content((px, py));
    let (c1x, c1y) = model_to_content((x, y));
    let d = Float.hypot(c1x -. c0x, c1y -. c0y);
    let steps = max(1, int_of_float(d /. cell));
    for (k in 1 to steps) {
      let f = float_of_int(k) /. float_of_int(steps);
      deposit(
        c0x +. (c1x -. c0x) *. f,
        c0y +. (c1y -. c0y) *. f,
        /* speed-scaled, capped: fast pulls churn harder */
        min(stroke_amp *. (0.4 +. d /. 24.), stroke_amp *. 2.),
      );
    };
  | (None, Some((x, y))) =>
    let (cx, cy) = model_to_content((x, y));
    deposit(cx, cy, stroke_amp);
  | (_, None) => last_geom := ((-1), (-1), 0., 0., 0.)
  };
  field := p;
  request_draw();
};
