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

/* field resolution, screen px. 7 was ~40% of the main thread during a
   score (a splash per act keeps the medium live; the step allocated a
   fresh field per substep and every dot was redrawn every frame) */
let cell = 9.;
/* live-tunable from the console while the feel is being dialed in:
     __waveTune("stiffness", 0.3)   __waveGet()
   stiffness = (c*dt/cell)^2, CFL-stable below ~0.5 */
let stiffness = ref(0.32);
let damping = ref(0.987); /* per substep: settles in ~1.5 s, not ~3 */
let grad_gain = ref(9.); /* field gradient -> dot displacement px */
let default_amp = 9.;
let stroke_amp = ref(3.6); /* drag-wake deposit per sample point */
let deposit_sigma = ref(1.6); /* injection kernel width, cells */
/* __waveTune("legacy", 1): v1 mechanics for A/B comparison —
   nearest-cell gradient sampling + 3x3 spike deposits. Toggling loads
   that mode's stock constants, stomping any hand tuning. */
let legacy = ref(false);
/* absorbing boundary: the grid is only viewport-sized, and its zero
   border is a hard wall — without absorption every front reflects off
   the panel edges and comes back (reads as motion restarting). The
   sponge ramps extra damping over the outer band so outgoing waves
   die there and the medium reads as infinite. 0 = reflecting walls. */
let sponge_k = ref(0.25);
let sponge_band = 14; /* cells */
/* steep wake gradients can push neighboring dots into each other (they
   visibly merge and 'vanish'); cap the displacement well under the
   14px dot pitch */
let disp_cap = ref(6.);
let apply_preset = (l: bool): unit =>
  if (l) {
    stiffness := 0.35;
    damping := 0.988;
    grad_gain := 5.5;
    stroke_amp := 3.2;
    sponge_k := 0.; /* v1 reflected; keep the A/B faithful */
  } else {
    stiffness := 0.32;
    damping := 0.991;
    grad_gain := 9.;
    stroke_amp := 3.6;
    deposit_sigma := 1.6;
    sponge_k := 0.25;
  };

/* forward refs: deposit/suction are defined below install_knobs */
let deposit_fwd: ref((float, float, float) => unit) = ref((_, _, _) => ());
let suction_fwd: ref(((float, float)) => unit) = ref(_ => ());
let knobs_installed = ref(false);
let n_full: ref(int) = ref(0)
and n_partial: ref(int) = ref(0)
and n_skip: ref(int) = ref(0)
and n_partial_area: ref(float) = ref(0.);

let install_knobs = (): unit =>
  if (! knobs_installed^) {
    knobs_installed := true;
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__waveTune",
      Js.Unsafe.callback((name: Js.t(Js.js_string), v: float) =>
        switch (Js.to_string(name)) {
        | "stiffness" => stiffness := max(0.05, min(0.45, v))
        | "damping" => damping := max(0.9, min(0.999, v))
        | "grad_gain" => grad_gain := v
        | "stroke_amp" => stroke_amp := v
        | "deposit_sigma" => deposit_sigma := max(0.6, min(4., v))
        | "legacy" =>
          legacy := v > 0.5;
          apply_preset(legacy^);
        | "sponge" => sponge_k := max(0., min(0.5, v))
        | "disp_cap" => disp_cap := max(1., v)
        | _ => ()
        }
      ),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__rippleCounters",
      Js.Unsafe.callback(() =>
        Js.string(
          Printf.sprintf(
            "full=%d partial=%d skip=%d partial_area=%.2f",
            n_full^,
            n_partial^,
            n_skip^,
            n_partial^ == 0
              ? 0. : n_partial_area^ /. float_of_int(n_partial^),
          ),
        )
      ),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__waveSuction",
      Js.Unsafe.callback((x: float, y: float) => suction_fwd^((x, y))),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__waveSplash",
      Js.Unsafe.callback((x: float, y: float, amp: float) => {
        let (cx, cy) = (pan_slack +. x *. zoom^, pan_slack +. y *. zoom^);
        deposit_fwd^(cx, cy, amp);
      }),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__waveGet",
      Js.Unsafe.callback(() =>
        Js.string(
          Printf.sprintf(
            "stiffness=%.3f damping=%.3f grad_gain=%.1f stroke_amp=%.1f deposit_sigma=%.1f sponge=%.2f disp_cap=%.1f legacy=%d",
            stiffness^,
            damping^,
            grad_gain^,
            stroke_amp^,
            deposit_sigma^,
            sponge_k^,
            disp_cap^,
            legacy^ ? 1 : 0,
          ),
        )
      ),
    );
  };

let gw: ref(int) = ref(0);
let gh: ref(int) = ref(0);
let u_cur: ref(array(float)) = ref([||]);
let u_prev: ref(array(float)) = ref([||]);
/* content-px position of cell (0,0) */
let origin_x: ref(float) = ref(0.);
let origin_y: ref(float) = ref(0.);
let sim_active: ref(bool) = ref(false);
let frame_parity: ref(bool) = ref(false);
/* the screen region drawn with live displacement last frame: with
   the viewport still, only the union of that and the current live
   region is cleared and redrawn (the lattice at rest is unchanged
   pixels). None forces a full redraw. */
let dirty_prev: ref(option((float, float, float, float))) = ref(None);
let last_fill: ref(string) = ref("");

/* content-px box of cells the medium is displacing (None: calm) */
let field_box = (): option((float, float, float, float)) =>
  if (gw^ == 0) {
    None;
  } else {
    let w = gw^
    and h = gh^;
    let u = u_cur^;
    let eps = 0.05 /. grad_gain^;
    let i0 = ref(w)
    and i1 = ref(-1)
    and j0 = ref(h)
    and j1 = ref(-1);
    for (j in 0 to h - 1) {
      let row = j * w;
      for (i in 0 to w - 1) {
        if (abs_float(u[row + i]) > eps) {
          if (i < i0^) {
            i0 := i;
          };
          if (i > i1^) {
            i1 := i;
          };
          if (j < j0^) {
            j0 := j;
          };
          if (j > j1^) {
            j1 := j;
          };
        };
      };
    };
    if (i1^ < 0) {
      None;
    } else {
      /* bilinear sampling reaches two cells out */
      Some((
        origin_x^ +. float_of_int(i0^ - 2) *. cell,
        origin_y^ +. float_of_int(j0^ - 2) *. cell,
        origin_x^ +. float_of_int(i1^ + 2) *. cell,
        origin_y^ +. float_of_int(j1^ + 2) *. cell,
      ));
    };
  };

let box_union = (a, b) =>
  switch (a, b) {
  | (None, x)
  | (x, None) => x
  | (Some((ax0, ay0, ax1, ay1)), Some((bx0, by0, bx1, by1))) =>
    Some((
      Float.min(ax0, bx0),
      Float.min(ay0, by0),
      Float.max(ax1, bx1),
      Float.max(ay1, by1),
    ))
  };
let last_step: ref(float) = ref(0.);

let zoom_ref = zoom; /* alias for clarity below */

let idx = (i: int, j: int): int => j * gw^ + i;

let ensure_grid = (cw: int, ch: int): unit => {
  /* the grid extends one sponge-band beyond the viewport on every side
     so absorption happens OFF-SCREEN: a deposit near the visible edge
     must land in live medium, not in the absorbing frame (a node
     removed near the panel edge used to have its suction eaten at
     birth) */
  let w =
    int_of_float(Float.ceil(float_of_int(cw) /. cell)) + 3 + 2 * sponge_band;
  let h =
    int_of_float(Float.ceil(float_of_int(ch) /. cell)) + 3 + 2 * sponge_band;
  if (w != gw^ || h != gh^) {
    /* PRESERVE in-flight waves across resizes: the pane height changes
       whenever the focus strip opens/closes — including the very frame
       a removal's suction is deposited (the dying node's panel closes),
       which used to zero the field and annihilate the ripple. Same
       origin, same cell indices; new cells start calm. */
    let ow = gw^
    and oh = gh^;
    let ou = u_cur^
    and op = u_prev^;
    let nu = Array.make(w * h, 0.)
    and np = Array.make(w * h, 0.);
    if (ow > 0) {
      for (j in 0 to min(h, oh) - 1) {
        for (i in 0 to min(w, ow) - 1) {
          nu[j * w + i] = ou[j * ow + i];
          np[j * w + i] = op[j * ow + i];
        };
      };
    };
    gw := w;
    gh := h;
    u_cur := nu;
    u_prev := np;
  };
};

/* keep the field pinned to content while the viewport scrolls: shift
   cells by whole-cell deltas */
let anchor = (sl: float, st: float): unit => {
  let margin = float_of_int(sponge_band) *. cell;
  let target_x = sl -. cell -. margin
  and target_y = st -. cell -. margin;
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

/* deposit a SMOOTH gaussian bump at a CONTENT-px point: sharp spikes
   disperse into gridded, anisotropic ringing on a coarse lattice (the
   v1 "not waterlike" artifact); wide smooth kernels launch clean
   circular fronts */
let deposit = (cx: float, cy: float, amp: float): unit =>
  if (gw^ > 0) {
    let fx = (cx -. origin_x^) /. cell
    and fy = (cy -. origin_y^) /. cell;
    let ci = int_of_float(Float.round(fx))
    and cj = int_of_float(Float.round(fy));
    if (legacy^) {
      for (j in cj - 1 to cj + 1) {
        for (i in ci - 1 to ci + 1) {
          if (i >= 0 && i < gw^ && j >= 0 && j < gh^) {
            let k = i == ci && j == cj ? 1.0 : 0.35;
            u_cur^[idx(i, j)] = u_cur^[idx(i, j)] +. amp *. k;
          };
        };
      };
    } else {
      let sg = deposit_sigma^;
      let r = int_of_float(Float.ceil(sg *. 2.5));
      for (j in cj - r to cj + r) {
        for (i in ci - r to ci + r) {
          if (i >= 0 && i < gw^ && j >= 0 && j < gh^) {
            let dx = float_of_int(i) -. fx
            and dy = float_of_int(j) -. fy;
            let d2 = (dx *. dx +. dy *. dy) /. (2. *. sg *. sg);
            u_cur^[idx(i, j)] =
              u_cur^[idx(i, j)] +. amp *. Float.exp(-. d2);
          };
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
      /* double-buffer: the previous field becomes the next (no
         allocation per substep) */
      let un = up;
      for (j in 1 to h - 2) {
        for (i in 1 to w - 2) {
          let k = j * w + i;
          let lap = u[k - 1] +. u[k + 1] +. u[k - w] +. u[k + w] -. 4. *. u[k];
          un[k] = damping^ *. (2. *. u[k] -. up[k] +. stiffness^ *. lap);
        };
      };
      if (sponge_k^ > 0.001) {
        /* absorb over the outer band (both fields, so no velocity
           kick); only the band's cells are visited, and with int
           comparisons (polymorphic min was a quarter of the step) */
        let band = sponge_band;
        let absorb = (i, j) => {
          let d = Int.min(Int.min(i, w - 1 - i), Int.min(j, h - 1 - j));
          if (d < band) {
            let t = 1. -. float_of_int(d) /. float_of_int(band);
            let f = 1. -. sponge_k^ *. t *. t;
            let k = j * w + i;
            un[k] = un[k] *. f;
            u[k] = u[k] *. f;
          };
        };
        for (j in 0 to band - 1) {
          for (i in 0 to w - 1) {
            absorb(i, j);
            absorb(i, h - 1 - j);
          };
        };
        for (j in band to h - 1 - band) {
          for (i in 0 to band - 1) {
            absorb(i, j);
            absorb(w - 1 - i, j);
          };
        };
      };
      u_prev := u;
      u_cur := un;
    };
    /* liveness: the medium is live while some cell could still move a
       dot visibly (peak |u| against the gradient gain; a summed
       criterion kept it "live" for seconds after the motion faded,
       redrawing every dot for nothing) */
    let m = ref(0.);
    let u = u_cur^;
    let n = Array.length(u);
    let i = ref(0);
    while (i^ < n) {
      let a = abs_float(u[i^]);
      if (a > m^) {
        m := a;
      };
      i := i^ + 3;
    };
    sim_active := m^ *. grad_gain^ > 0.08;
  };
};

/* field gradient at a content-px point, BILINEARLY interpolated:
   nearest-cell sampling made neighboring dots snap between discrete
   gradients as a wave passed (the v1 jank) */
let displacement_at = (cx: float, cy: float): (float, float) =>
  if (gw^ == 0) {
    (0., 0.);
  } else if (legacy^) {
    let i = int_of_float(Float.round((cx -. origin_x^) /. cell))
    and j = int_of_float(Float.round((cy -. origin_y^) /. cell));
    if (i < 1 || i >= gw^ - 1 || j < 1 || j >= gh^ - 1) {
      (0., 0.);
    } else {
      let u = u_cur^;
      let gx = (u[idx(i + 1, j)] -. u[idx(i - 1, j)]) /. 2.
      and gy = (u[idx(i, j + 1)] -. u[idx(i, j - 1)]) /. 2.;
      (grad_gain^ *. gx, grad_gain^ *. gy);
    };
  } else {
    let fx = (cx -. origin_x^) /. cell
    and fy = (cy -. origin_y^) /. cell;
    let i0 = int_of_float(Float.floor(fx))
    and j0 = int_of_float(Float.floor(fy));
    if (i0 < 1 || i0 >= gw^ - 2 || j0 < 1 || j0 >= gh^ - 2) {
      (0., 0.);
    } else {
      let tx = fx -. float_of_int(i0)
      and ty = fy -. float_of_int(j0);
      let u = u_cur^;
      let grad = (i: int, j: int): (float, float) => (
        (u[idx(i + 1, j)] -. u[idx(i - 1, j)]) /. 2.,
        (u[idx(i, j + 1)] -. u[idx(i, j - 1)]) /. 2.,
      );
      let (g00x, g00y) = grad(i0, j0)
      and (g10x, g10y) = grad(i0 + 1, j0)
      and (g01x, g01y) = grad(i0, j0 + 1)
      and (g11x, g11y) = grad(i0 + 1, j0 + 1);
      let lerp = (a, b, t) => a +. (b -. a) *. t;
      let gx = lerp(lerp(g00x, g10x, tx), lerp(g01x, g11x, tx), ty)
      and gy = lerp(lerp(g00y, g10y, tx), lerp(g01y, g11y, tx), ty);
      let dx = grad_gain^ *. gx
      and dy = grad_gain^ *. gy;
      let n = Float.hypot(dx, dy);
      n > disp_cap^
        ? (dx *. disp_cap^ /. n, dy *. disp_cap^ /. n) : (dx, dy);
    };
  };

/* drag wake: injection along the path between successive drag samples */
let field: ref(option((float, float))) =
  ref(None: option((float, float)));

/* ---- edge pulses: data visibly flowing ----
   A bright dot with a fading tail travels a function edge's bezier
   (model coords) when that function's samples change; on arrival it
   deposits a little energy into the medium. */
type pulse = {
  p0: (float, float),
  p1: (float, float),
  p2: (float, float),
  p3: (float, float),
  t0: float,
};
let pulses: ref(list(pulse)) = ref([]);
let pulse_ms = 650.;

/* removal suction, two-phase: the WAVE MEDIUM cannot pull dots inward
   first — any deposit's leading front propagates OUTWARD and dots ride
   the front (out-in-out, as observed). So the pull phase is ANALYTIC:
   an easing radial displacement drawn directly onto the dots (no field
   involvement), and only the release plants a positive bump in the
   medium for the outgoing rebound ring. */
type suction_t = {
  su_p: (float, float), /* model coords */
  su_t0: float,
};
let suctions: ref(list(suction_t)) = ref([]);
let suction_pull_ms = 380.;
let suction_pull_px = 8.5; /* peak inward displacement */
let suction_pull_r = 110.; /* gaussian reach, screen px */
let suction_rebound = 8.; /* released bump amplitude */

let bezier =
    (
      ((x0, y0), (x1, y1), (x2, y2), (x3, y3)): (
        (float, float),
        (float, float),
        (float, float),
        (float, float),
      ),
      t: float,
    )
    : (float, float) => {
  let mt = 1. -. t;
  let a = mt *. mt *. mt
  and b = 3. *. mt *. mt *. t
  and c = 3. *. mt *. t *. t
  and d = t *. t *. t;
  (
    a *. x0 +. b *. x1 +. c *. x2 +. d *. x3,
    a *. y0 +. b *. y1 +. c *. y2 +. d *. y3,
  );
};

let raf_running: ref(bool) = ref(false);
let draw_queued: ref(bool) = ref(false);

/* Dots render from a pre-rasterized sprite via drawImage instead of
   per-dot arc fills: a 1.5px arc's apparent brightness swings ~2x with
   its subpixel phase, so dots visibly twinkle while a wave displaces
   them; bilinear resampling of a soft sprite keeps peak brightness
   stable in motion (and drawImage is cheaper than path fill). */
let dot_sprite: ref(option(Js.Unsafe.any)) = ref(None);
let dot_sprite_key: ref(string) = ref("");
let sprite_size = 8.; /* css px, dot centered */
let get_dot_sprite = (fill: string, dpr: float): Js.Unsafe.any => {
  let key = Printf.sprintf("%s|%.2f", fill, dpr);
  switch (dot_sprite^) {
  | Some(c) when dot_sprite_key^ == key => c
  | _ =>
    let doc = Js.Unsafe.global##.document;
    /* resolve the fill to rgb so the gradient can fade to a
       same-color transparent (fading to rgba(0,0,0,0) darkens) */
    let probe =
      Js.Unsafe.meth_call(
        doc,
        "createElement",
        [|Js.Unsafe.inject(Js.string("canvas"))|],
      );
    Js.Unsafe.set(probe, "width", 1);
    Js.Unsafe.set(probe, "height", 1);
    let pctx =
      Js.Unsafe.meth_call(
        probe,
        "getContext",
        [|Js.Unsafe.inject(Js.string("2d"))|],
      );
    Js.Unsafe.set(pctx, "fillStyle", Js.string(fill));
    let _ =
      Js.Unsafe.meth_call(
        pctx,
        "fillRect",
        [|
          Js.Unsafe.inject(0),
          Js.Unsafe.inject(0),
          Js.Unsafe.inject(1),
          Js.Unsafe.inject(1),
        |],
      );
    let px: Js.t(Js.Unsafe.any) =
      Js.Unsafe.meth_call(
        pctx,
        "getImageData",
        [|
          Js.Unsafe.inject(0),
          Js.Unsafe.inject(0),
          Js.Unsafe.inject(1),
          Js.Unsafe.inject(1),
        |],
      );
    let data = Js.Unsafe.get(px, "data");
    let ch = (i: int): float => Js.Unsafe.get(data, i);
    let ch = (i: int): int => int_of_float(ch(i));
    let (r, g, b) = (ch(0), ch(1), ch(2));
    let c =
      Js.Unsafe.meth_call(
        doc,
        "createElement",
        [|Js.Unsafe.inject(Js.string("canvas"))|],
      );
    let px_size = int_of_float(Float.ceil(sprite_size *. dpr));
    Js.Unsafe.set(c, "width", px_size);
    Js.Unsafe.set(c, "height", px_size);
    let cx =
      Js.Unsafe.meth_call(
        c,
        "getContext",
        [|Js.Unsafe.inject(Js.string("2d"))|],
      );
    let mid = float_of_int(px_size) /. 2.;
    /* solid core matching the old 0.75px arc, short soft skirt */
    let grad =
      Js.Unsafe.meth_call(
        cx,
        "createRadialGradient",
        [|
          Js.Unsafe.inject(mid),
          Js.Unsafe.inject(mid),
          Js.Unsafe.inject(0.),
          Js.Unsafe.inject(mid),
          Js.Unsafe.inject(mid),
          Js.Unsafe.inject(1.25 *. dpr),
        |],
      );
    let stop = (at: float, a: float) => {
      let _ =
        Js.Unsafe.meth_call(
          grad,
          "addColorStop",
          [|
            Js.Unsafe.inject(at),
            Js.Unsafe.inject(
              Js.string(Printf.sprintf("rgba(%d,%d,%d,%f)", r, g, b, a)),
            ),
          |],
        );
      ();
    };
    stop(0., 1.);
    stop(0.6, 1.);
    stop(1., 0.);
    Js.Unsafe.set(cx, "fillStyle", grad);
    let _ =
      Js.Unsafe.meth_call(
        cx,
        "fillRect",
        [|
          Js.Unsafe.inject(0),
          Js.Unsafe.inject(0),
          Js.Unsafe.inject(px_size),
          Js.Unsafe.inject(px_size),
        |],
      );
    dot_sprite := Some(c);
    dot_sprite_key := key;
    c;
  };
};
/* geometry of the last static draw, to skip redundant repaints */
let last_geom: ref((int, int, float, float, float)) =
  ref((0, 0, 0., 0., 0.));
let geom_unchanged = (cw, ch, sl, st, z): bool =>
  last_geom^ == (cw, ch, sl, st, z);

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
  install_knobs();
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-dots")) {
  | None =>
    /* the element can be briefly absent mid-patch; if work is pending,
       retry next frame instead of silently dropping the rAF chain */
    if (sim_active^ || suctions^ != [] || pulses^ != []) {
      let _ =
        Js.Unsafe.meth_call(
          Js.Unsafe.global##.window,
          "requestAnimationFrame",
          [|Js.Unsafe.inject(Js.Unsafe.callback(() => draw()))|],
        );
      ();
    }
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
    /* suction lifecycle: while pulling, the analytic term below moves
       the dots; on expiry, plant the rebound bump in the medium */
    if (suctions^ != []) {
      List.iter(
        (su: suction_t) =>
          if (t -. su.su_t0 > suction_pull_ms) {
            let (mx, my) = su.su_p;
            let (cx, cy) = model_to_content((mx, my));
            let sg = deposit_sigma^;
            deposit_sigma := 2.6;
            deposit(cx, cy, suction_rebound);
            deposit_sigma := sg;
            CanvasLog.log(
              Printf.sprintf(
                "suction rebound: content=(%.0f,%.0f) cell=(%d,%d) grid=(%d,%d)",
                cx,
                cy,
                int_of_float(Float.round((cx -. origin_x^) /. cell)),
                int_of_float(Float.round((cy -. origin_y^) /. cell)),
                gw^,
                gh^,
              ),
            );
          },
        suctions^,
      );
      suctions :=
        List.filter(
          (su: suction_t) => t -. su.su_t0 <= suction_pull_ms,
          suctions^,
        );
    };
    /* the medium runs at half rate: a wave field at 30 Hz reads the
       same, and the full redraw of every dot is the expensive part */
    frame_parity := ! frame_parity^;
    let skip = frame_parity^ && geom_unchanged(cw, ch, sl, st, z);
    if (skip) {
      incr(n_skip);
    };
    if (sim_active^ && !skip) {
      let dt_ms = 8.;
      let elapsed = last_step^ == 0. ? dt_ms : min(48., t -. last_step^);
      step_sim(max(1, min(6, int_of_float(elapsed /. dt_ms))));
      last_step := t;
    } else if (! sim_active^) {
      last_step := t;
    };
    let geom = (cw, ch, sl, st, z);
    if (!skip
        && (
          sim_active^ || pulses^ != [] || suctions^ != [] || geom != last_geom^
        )) {
      let geom_changed = geom != last_geom^;
      last_geom := geom;
      let ctx =
        Js.Unsafe.meth_call(
          el,
          "getContext",
          [|Js.Unsafe.inject(Js.string("2d"))|],
        );
      let mw = float_of_int(cw)
      and mh = float_of_int(ch);
      let fmin = (a: float, b: float) => a < b ? a : b
      and fmax = (a: float, b: float) => a > b ? a : b;
      /* the live region this frame, screen px: the medium's active
         cells, each suction's reach, and every pulse's head-to-tail
         span (pulse positions are computed again below; they are few) */
      let live_box = {
        let su_box =
          List.fold_left(
            (acc, su: suction_t) => {
              let age = (t -. su.su_t0) /. suction_pull_ms;
              if (age >= 0. && age <= 1.) {
                let (mx, my) = su.su_p;
                let (cx, cy) = model_to_content((mx, my));
                let r = suction_pull_r *. 2.5;
                box_union(acc, Some((cx -. r, cy -. r, cx +. r, cy +. r)));
              } else {
                acc;
              };
            },
            None,
            suctions^,
          );
        let pulse_box =
          List.fold_left(
            (acc, p: pulse) => {
              let tt = (t -. p.t0) /. pulse_ms;
              List.fold_left(
                (acc, back) => {
                  let tk = fmax(0., tt -. back);
                  let (mx, my) = bezier((p.p0, p.p1, p.p2, p.p3), tk);
                  let cx = mx *. z +. pan_slack
                  and cy = my *. z +. pan_slack;
                  box_union(
                    acc,
                    Some((cx -. 4., cy -. 4., cx +. 4., cy +. 4.)),
                  );
                },
                acc,
                [0., 0.06, 0.12, 0.18],
              );
            },
            None,
            pulses^,
          );
        switch (box_union(box_union(field_box(), su_box), pulse_box)) {
        | None => None
        | Some((x0, y0, x1, y1)) =>
          /* content -> screen, padded by a dot's sprite plus the
             largest displacement, clipped to the viewport */
          let pad = sprite_size /. 2. +. disp_cap^ +. suction_pull_px +. 2.;
          let bx0 = fmax(0., x0 -. sl -. pad)
          and by0 = fmax(0., y0 -. st -. pad)
          and bx1 = fmin(mw, x1 -. sl +. pad)
          and by1 = fmin(mh, y1 -. st +. pad);
          bx0 < bx1 && by0 < by1 ? Some((bx0, by0, bx1, by1)) : None;
        };
      };
      let fill: Js.t(Js.js_string) =
        Js.Unsafe.meth_call(
          Js.Unsafe.global##.window,
          "getComputedStyle",
          [|Js.Unsafe.inject(scroll)|],
        )##getPropertyValue(
          Js.string("--BR1"),
        );
      let fill = Js.to_string(fill);
      let fill = fill == "" ? "#d8c9a3" : fill;
      let full = geom_changed || fill != last_fill^;
      last_fill := fill;
      if (full) {
        incr(n_full);
      } else {
        incr(n_partial);
      };
      let region =
        full ? Some((0., 0., mw, mh)) : box_union(live_box, dirty_prev^);
      dirty_prev := live_box;
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
      let (rx0, ry0, rx1, ry1) =
        Option.value(region, ~default=(0., 0., 0., 0.));
      if (!full) {
        n_partial_area :=
          n_partial_area^ +. (rx1 -. rx0) *. (ry1 -. ry0) /. (mw *. mh);
      };
      let _ = Js.Unsafe.meth_call(ctx, "save", [||]);
      let _ = Js.Unsafe.meth_call(ctx, "beginPath", [||]);
      let _ =
        Js.Unsafe.meth_call(
          ctx,
          "rect",
          [|
            Js.Unsafe.inject(rx0),
            Js.Unsafe.inject(ry0),
            Js.Unsafe.inject(rx1 -. rx0),
            Js.Unsafe.inject(ry1 -. ry0),
          |],
        );
      let _ = Js.Unsafe.meth_call(ctx, "clip", [||]);
      let _ =
        Js.Unsafe.meth_call(
          ctx,
          "clearRect",
          [|
            Js.Unsafe.inject(rx0),
            Js.Unsafe.inject(ry0),
            Js.Unsafe.inject(rx1 -. rx0),
            Js.Unsafe.inject(ry1 -. ry0),
          |],
        );
      Js.Unsafe.coerce(ctx)##.fillStyle := Js.string(fill);
      let sprite = get_dot_sprite(fill, dpr);
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
        let d = fmin(v, extent -. v);
        d <= 0. ? 0. : fmin(1., d /. edge);
      };
      /* board model coord -> screen px */
      let to_screen_x = (m: float): float => m *. z +. pan_slack -. sl
      and to_screen_y = (m: float): float => m *. z +. pan_slack -. st;
      /* per-frame analytic suction terms: (content center, strength) */
      let su_terms =
        List.filter_map(
          (su: suction_t) => {
            let age = (t -. su.su_t0) /. suction_pull_ms;
            if (age >= 0. && age <= 1.) {
              let (mx, my) = su.su_p;
              let (scx, scy) = model_to_content((mx, my));
              /* fast ease-in then hold; release is the field rebound */
              let e = age < 0.25 ? age /. 0.25 : 1.;
              Some((scx, scy, suction_pull_px *. e));
            } else {
              None;
            };
          },
          suctions^,
        );
      let draw_pass = (step: float, alpha: float, ~skip_coarse: bool) => {
        let reach = sprite_size /. 2. +. disp_cap^ +. suction_pull_px;
        let m0x = (rx0 -. reach +. sl -. pan_slack) /. z
        and m1x = (rx1 +. reach +. sl -. pan_slack) /. z;
        let m0y = (ry0 -. reach +. st -. pan_slack) /. z
        and m1y = (ry1 +. reach +. st -. pan_slack) /. z;
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
              /* analytic inward pull toward any active suction */
              let (dx, dy) =
                List.fold_left(
                  ((ax, ay), (scx, scy, amp)) => {
                    let ddx = scx -. cx
                    and ddy = scy -. cy;
                    let d = Float.hypot(ddx, ddy);
                    /* gaussian falloff, sign made explicit: `-. x ** 2.`
                       parses as `(-. x) ** 2.` (positive!) and blew every
                       far dot off-screen */
                    let q = d /. suction_pull_r;
                    let w = amp *. Float.exp(-. (q *. q));
                    /* a very close dot may not cross the sink */
                    let w = min(w, d);
                    d < 1.
                      ? (ax, ay) : (ax +. ddx /. d *. w, ay +. ddy /. d *. w);
                  },
                  (dx, dy),
                  su_terms,
                );
              let sx = cx +. dx -. sl
              and sy = cy +. dy -. st;
              let a = alpha *. fade1(sx, mw) *. fade1(sy, mh);
              if (a > 0.01) {
                Js.Unsafe.coerce(ctx)##.globalAlpha := a;
                let half = sprite_size /. 2.;
                let _ =
                  Js.Unsafe.meth_call(
                    ctx,
                    "drawImage",
                    [|
                      Js.Unsafe.inject(sprite),
                      Js.Unsafe.inject(sx -. half),
                      Js.Unsafe.inject(sy -. half),
                      Js.Unsafe.inject(sprite_size),
                      Js.Unsafe.inject(sprite_size),
                    |],
                  );
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
      /* pulses ride on top of the lattice */
      let (live_pulses, done_pulses) =
        List.partition((p: pulse) => t -. p.t0 < pulse_ms, pulses^);
      pulses := live_pulses;
      List.iter(
        (p: pulse) => {
          let (mx, my) = bezier((p.p0, p.p1, p.p2, p.p3), 1.);
          let (cx, cy) = model_to_content((mx, my));
          deposit(cx, cy, 3.5);
        },
        done_pulses,
      );
      List.iter(
        (p: pulse) => {
          let tt = (t -. p.t0) /. pulse_ms;
          /* head + three tail samples fading behind */
          List.iteri(
            (k, back) => {
              let tk = max(0., tt -. back);
              let (mx, my) = bezier((p.p0, p.p1, p.p2, p.p3), tk);
              let cx = mx *. z +. pan_slack
              and cy = my *. z +. pan_slack;
              let (ddx, ddy) = displacement_at(cx, cy);
              let sx = cx +. ddx -. sl
              and sy = cy +. ddy -. st;
              let a =
                (k == 0 ? 0.9 : 0.5 -. 0.13 *. float_of_int(k))
                *. fade1(sx, mw)
                *. fade1(sy, mh);
              if (a > 0.02) {
                Js.Unsafe.coerce(ctx)##.globalAlpha := a;
                let _ = Js.Unsafe.meth_call(ctx, "beginPath", [||]);
                let _ =
                  Js.Unsafe.meth_call(
                    ctx,
                    "arc",
                    [|
                      Js.Unsafe.inject(sx),
                      Js.Unsafe.inject(sy),
                      Js.Unsafe.inject(k == 0 ? 1.8 : 1.2),
                      Js.Unsafe.inject(0.),
                      Js.Unsafe.inject(2. *. Float.pi),
                    |],
                  );
                let _ = Js.Unsafe.meth_call(ctx, "fill", [||]);
                ();
              };
            },
            List.init(4, k => float_of_int(k) *. 0.06),
          );
        },
        live_pulses,
      );
      Js.Unsafe.coerce(ctx)##.globalAlpha := 1.;
      let _ = Js.Unsafe.meth_call(ctx, "restore", [||]);
      ();
    };
    if (sim_active^ || pulses^ != [] || suctions^ != [] || zoom_settling) {
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

/* launch a pulse along an edge's bezier (model-space control points) */
let pulse_edge =
    (
      p0: (float, float),
      p1: (float, float),
      p2: (float, float),
      p3: (float, float),
    )
    : unit => {
  pulses :=
    [
      {
        p0,
        p1,
        p2,
        p3,
        t0: now(),
      },
      ...pulses^,
    ];
  request_draw();
};

/* splash: one impulse into the medium at a model-space point */
let splash = (~amp: float=default_amp, (x, y): (float, float)): unit => {
  let (cx, cy) = model_to_content((x, y));
  deposit(cx, cy, amp);
  request_draw();
};

let suction = ((x, y): (float, float)): unit => {
  suctions :=
    [
      {
        su_p: (x, y),
        su_t0: now(),
      },
      ...suctions^,
    ];
  sim_active := true;
  request_draw();
};
suction_fwd := suction;
deposit_fwd :=
  (
    (cx, cy, amp) => {
      deposit(cx, cy, amp);
      request_draw();
    }
  );

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
        min(stroke_amp^ *. (0.4 +. d /. 24.), stroke_amp^ *. 2.),
      );
    };
  | (None, Some((x, y))) =>
    let (cx, cy) = model_to_content((x, y));
    deposit(cx, cy, stroke_amp^);
  | (_, None) => last_geom := ((-1), (-1), 0., 0., 0.)
  };
  field := p;
  request_draw();
};
