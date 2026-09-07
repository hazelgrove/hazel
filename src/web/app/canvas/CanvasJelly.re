open Js_of_ocaml;

/* CanvasJelly — inertia for the module metaballs.

   Each hull circle is a damped spring chasing its target (the position
   the layout wants). When nodes move — drags, re-layouts, collapse —
   targets jump and the circles lag, overshoot, and settle; since the
   blob boundary is the union of the circles, the hull visibly
   stretches and wobbles like jelly. Bigger circles are heavier
   (softer springs), so parents slosh behind their contents.

   Targets are pushed by the render and by the drag-time layout applier
   (CanvasView.hull_targets); this module owns the circles' cx/cy while
   any spring is unsettled. */

type spring = {
  mutable x: float,
  mutable y: float,
  mutable vx: float,
  mutable vy: float,
  mutable tx: float,
  mutable ty: float,
  mutable r: float,
  mutable seen: bool,
  /* a scored relayout: the circle GLIDES from (gx, gy) to its target on
     the nodes' own schedule (start, duration, ease-out-expo) instead of
     springing — springs led the nodes by most of a second and overshot
     ("the metaballs are out of sync with the nodes") */
  mutable glide: option((float, float, float, float)) /* gx gy t0 dur */
};

/* ease-out-expo, the movers' curve (Animation.easeOutExpo) */
let ease = (u: float): float =>
  u >= 1. ? 1. : 1. -. Float.pow(2., (-10.) *. u);

let springs: Hashtbl.t(string, spring) = Hashtbl.create(64);
let running = ref(false);
let last_t = ref(0.);

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let set_attr = (el, name: string, v: float): unit =>
  ignore(
    Js.Unsafe.meth_call(
      el,
      "setAttribute",
      [|
        Js.Unsafe.inject(Js.string(name)),
        Js.Unsafe.inject(Js.string(Printf.sprintf("%.1f", v))),
      |],
    ),
  );

let rec tick = (): unit => {
  let t = now();
  let dt = min(0.04, max(0.001, (t -. last_t^) /. 1000.));
  last_t := t;
  let energy = ref(0.);
  let dead: ref(list(string)) = ref([]);
  Hashtbl.iter(
    (id, s) => {
      switch (s.glide) {
      | Some((gx, gy, t0, dur)) =>
        let u = dur <= 0. ? 1. : (t -. t0) /. dur;
        if (u >= 1.) {
          s.x = s.tx;
          s.y = s.ty;
          s.vx = 0.;
          s.vy = 0.;
          s.glide = None;
        } else if (u > 0.) {
          let e = ease(u);
          s.x = gx +. (s.tx -. gx) *. e;
          s.y = gy +. (s.ty -. gy) *. e;
        };
      | None =>
        /* stiffness scales inversely with radius: big = heavy = sloshy */
        let k = 260. *. (42. /. max(20., s.r));
        let c = 2. *. Float.sqrt(k) *. 0.62; /* underdamped: wobble */
        let ax = k *. (s.tx -. s.x) -. c *. s.vx
        and ay = k *. (s.ty -. s.y) -. c *. s.vy;
        s.vx = s.vx +. ax *. dt;
        s.vy = s.vy +. ay *. dt;
        s.x = s.x +. s.vx *. dt;
        s.y = s.y +. s.vy *. dt;
      };
      energy :=
        energy^
        +. abs_float(s.tx -. s.x)
        +. abs_float(s.ty -. s.y)
        +. 0.05
        *. (abs_float(s.vx) +. abs_float(s.vy));
      switch (Util.JsUtil.get_elem_by_id_opt(id)) {
      | Some(el) =>
        set_attr(el, "cx", s.x);
        set_attr(el, "cy", s.y);
      | None => dead := [id, ...dead^]
      };
    },
    springs,
  );
  List.iter(id => Hashtbl.remove(springs, id), dead^);
  if (energy^ > 0.8 && Hashtbl.length(springs) > 0) {
    let _ =
      Js.Unsafe.meth_call(
        Js.Unsafe.global##.window,
        "requestAnimationFrame",
        [|Js.Unsafe.inject(Js.Unsafe.callback(() => tick()))|],
      );
    ();
  } else {
    /* settle exactly on target before sleeping */
    Hashtbl.iter(
      (id, s) => {
        s.x = s.tx;
        s.y = s.ty;
        s.vx = 0.;
        s.vy = 0.;
        switch (Util.JsUtil.get_elem_by_id_opt(id)) {
        | Some(el) =>
          set_attr(el, "cx", s.x);
          set_attr(el, "cy", s.y);
        | None => ()
        };
      },
      springs,
    );
    running := false;
  };
};

let kick = (): unit =>
  if (! running^) {
    running := true;
    last_t := now();
    let _ =
      Js.Unsafe.meth_call(
        Js.Unsafe.global##.window,
        "requestAnimationFrame",
        [|Js.Unsafe.inject(Js.Unsafe.callback(() => tick()))|],
      );
    ();
  };

/* push fresh targets; unseen circles are born AT their target (no
   fly-in), known circles keep their current state and chase */
let set_targets =
    /* Some((start, dur)): a staged beat — existing circles glide on the
       nodes' schedule; None: springs (drags, idle relayouts) */
    (
      ~glide: option((float, float))=None,
      ts: list((string, CanvasLayout.pos, float)),
    )
    : unit => {
  Hashtbl.iter((_, s) => s.seen = false, springs);
  List.iter(
    ((id, p: CanvasLayout.pos, r)) =>
      switch (Hashtbl.find_opt(springs, id)) {
      | Some(s) =>
        let moved =
          abs_float(p.x -. s.tx) > 0.5 || abs_float(p.y -. s.ty) > 0.5;
        if (moved) {
          switch (glide) {
          | Some((t0, dur)) => s.glide = Some((s.x, s.y, t0, dur))
          | None => ()
          };
        };
        s.tx = p.x;
        s.ty = p.y;
        s.r = r;
        s.seen = true;
      | None =>
        Hashtbl.replace(
          springs,
          id,
          {
            x: p.x,
            y: p.y,
            vx: 0.,
            vy: 0.,
            tx: p.x,
            ty: p.y,
            r,
            seen: true,
            glide: None,
          },
        )
      },
    ts,
  );
  /* circles gone from the layout: drop their springs */
  let dead =
    Hashtbl.fold((id, s, acc) => s.seen ? acc : [id, ...acc], springs, []);
  List.iter(id => Hashtbl.remove(springs, id), dead);
  kick();
};
