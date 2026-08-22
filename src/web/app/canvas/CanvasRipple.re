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
/* a moving repulsion field (the dragged node's bow wave): dots yield
   around this point while it is set */
let field: ref(option((float, float))) =
  ref(None: option((float, float)));
let field_amp = 7.;
let field_sigma = 48.;
let raf_running: ref(bool) = ref(false);
let draw_queued: ref(bool) = ref(false);
/* geometry of the last static draw, to skip redundant repaints */
let last_geom: ref((int, int, float, float)) = ref((0, 0, 0., 0.));

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
    let root = el##.parentElement;
    let t = now();
    let live = List.filter((r: ripple) => t -. r.start < duration, ripples^);
    ripples := live;
    let pitch = Option.value(~default=14., css_var(root, "--dot-pitch"));
    let r_dot = Option.value(~default=0.75, css_var(root, "--dot-r"));
    let w: int = el##.width
    and h: int = el##.height;
    let dpr: float = Js.Unsafe.coerce(Js.Unsafe.global)##.devicePixelRatio;
    let mw = float_of_int(w) /. dpr
    and mh = float_of_int(h) /. dpr;
    let geom = (w, h, pitch, r_dot);
    if (live != [] || field^ != None || geom != last_geom^) {
      last_geom := geom;
      let ctx =
        Js.Unsafe.meth_call(
          el,
          "getContext",
          [|Js.Unsafe.inject(Js.string("2d"))|],
        );
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
          [|Js.Unsafe.inject(root)|],
        )##getPropertyValue(
          Js.string("--BR1"),
        );
      let fill = Js.to_string(fill);
      Js.Unsafe.coerce(ctx)##.fillStyle :=
        Js.string(fill == "" ? "#d8c9a3" : fill);
      let two_pi = 2. *. Float.pi;
      let nx = int_of_float(Float.ceil(mw /. pitch))
      and ny = int_of_float(Float.ceil(mh /. pitch));
      for (iy in 0 to ny) {
        for (ix in 0 to nx) {
          /* dot centers ON lattice multiples — where node centers snap
             (the old CSS background's half-pitch tile offset did this) */
          let x = float_of_int(ix) *. pitch
          and y = float_of_int(iy) *. pitch;
          let (dx, dy) =
            List.fold_left(
              ((ax, ay), rp: ripple) => {
                let age = t -. rp.start;
                let ddx = x -. rp.rx
                and ddy = y -. rp.ry;
                let d = max(1., Float.hypot(ddx, ddy));
                let u = (d -. speed *. age) /. ring_w;
                let envelope =
                  rp.r_amp
                  *. Float.exp(-. (u *. u))
                  *. (1. -. age /. duration);
                (ax +. ddx /. d *. envelope, ay +. ddy /. d *. envelope);
              },
              (0., 0.),
              live,
            );
          let (dx, dy) =
            switch (field^) {
            | None => (dx, dy)
            | Some((fx, fy)) =>
              let ddx = x -. fx
              and ddy = y -. fy;
              let d = max(1., Float.hypot(ddx, ddy));
              let g = d /. field_sigma;
              let push = field_amp *. Float.exp(-. (g *. g));
              (dx +. ddx /. d *. push, dy +. ddy /. d *. push);
            };
          let _ = Js.Unsafe.meth_call(ctx, "beginPath", [||]);
          let _ =
            Js.Unsafe.meth_call(
              ctx,
              "arc",
              [|
                Js.Unsafe.inject(x +. dx),
                Js.Unsafe.inject(y +. dy),
                Js.Unsafe.inject(r_dot),
                Js.Unsafe.inject(0.),
                Js.Unsafe.inject(two_pi),
              |],
            );
          let _ = Js.Unsafe.meth_call(ctx, "fill", [||]);
          ();
        };
      };
    };
    if (live != [] || field^ != None) {
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
    last_geom := ((-1), (-1), 0., 0.);
  };
  request_draw();
};
