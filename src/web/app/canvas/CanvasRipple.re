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

type ripple = {
  rx: float,
  ry: float,
  start: float,
  r_amp: float,
};

let ripples: ref(list(ripple)) = ref([]);

/* view state, pushed in by CanvasSidebar each render (and imperatively
   during a pinch): the dots canvas is VIEWPORT-fixed and needs the
   board->screen mapping to draw the lattice window under it */
let zoom: ref(float) = ref(1.);
let pan_slack = 392.; /* keep in sync with CanvasSidebar.pan_slack */
/* a moving repulsion field (the dragged node's bow wave): dots yield
   around this point while it is set */
let field: ref(option((float, float))) =
  ref(None: option((float, float)));
let field_amp = 7.;
let field_sigma = 48.;
let raf_running: ref(bool) = ref(false);
let draw_queued: ref(bool) = ref(false);
/* geometry of the last static draw, to skip redundant repaints */
let last_geom: ref((int, int, float, float, float)) =
  ref((0, 0, 0., 0., 0.));

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

/* wave parameters (model px / ms) */
let duration = 1400.;
let speed = 0.38; /* px per ms: ring radius at t is speed * t */
let ring_w = 46.; /* gaussian half-width of the compression ring */
let default_amp = 9.; /* peak radial displacement (negative = suction) */

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
    let live = List.filter((r: ripple) => t -. r.start < duration, ripples^);
    ripples := live;
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
    let geom = (cw, ch, sl, st, z);
    if (live != [] || field^ != None || geom != last_geom^) {
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
      let displaced = (x: float, y: float): (float, float) => {
        let (dx, dy) =
          List.fold_left(
            ((ax, ay), rp: ripple) => {
              let age = t -. rp.start;
              let ddx = x -. rp.rx
              and ddy = y -. rp.ry;
              let d = max(1., Float.hypot(ddx, ddy));
              let u = (d -. speed *. age) /. ring_w;
              let envelope =
                rp.r_amp *. Float.exp(-. (u *. u)) *. (1. -. age /. duration);
              (ax +. ddx /. d *. envelope, ay +. ddy /. d *. envelope);
            },
            (0., 0.),
            live,
          );
        switch (field^) {
        | None => (dx, dy)
        | Some((fx, fy)) =>
          let ddx = x -. fx
          and ddy = y -. fy;
          let d = max(1., Float.hypot(ddx, ddy));
          let gg = d /. field_sigma;
          let push = field_amp *. Float.exp(-. (gg *. gg));
          (dx +. ddx /. d *. push, dy +. ddy /. d *. push);
        };
      };
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
              let (dx, dy) = displaced(x, y);
              let sx = to_screen_x(x +. dx)
              and sy = to_screen_y(y +. dy);
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
    if (live != [] || field^ != None || zoom_settling) {
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

/* splash a compression wave outward from a model-space point */
let splash = (~amp: float=default_amp, (x, y): (float, float)): unit => {
  ripples :=
    [
      {
        rx: x,
        ry: y,
        start: now(),
        r_amp: amp,
      },
      ...ripples^,
    ];
  request_draw();
};

/* drag bow wave: set while a node drag is live, clear on drop. Clearing
   invalidates the geometry cache so one final draw settles the dots. */
let set_field = (p: option((float, float))): unit => {
  field := p;
  if (p == None) {
    last_geom := ((-1), (-1), 0., 0., 0.);
  };
  request_draw();
};
