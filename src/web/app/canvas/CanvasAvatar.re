/* CanvasAvatar — the agent's second look: a small constellation rig
   (three vertices, three edges) with MOODS, next to the minimal "@"
   glyph. plans/agent-canvas-avatar-spec.md is the spec; this is its
   first pass: idle breathes and turns slowly, thinking spins and swaps
   emoji on the vertices, travel turns the leading vertex toward the
   destination behind a light beam and stretches, arrival squashes and
   settles, editing grows onto the node, drawing/erasing lead with the
   vertex as a pen, errors jitter red.

   The rig lives INSIDE .avatar-body (the WAAPI ride transform's target)
   on its own layers, so state transforms never fight the path:
     .avatar-body  (ride: WAAPI translate)
       .rig-heading  (CSS: --heading rotation, mood scale/squash, edit offset)
         svg.rig > g.rig-spin (CSS: spin / breathe)  + beam + glow
   Mood classes and --heading are written straight to the body element,
   which the view renders with a constant class, so re-renders leave them
   alone. The look is a toolbar toggle persisted in localStorage. */

open Js_of_ocaml;
open Virtual_dom.Vdom;

let look_key = "constellation.avatarLook";

let storage_get = (k: string): option(string) =>
  switch (
    Js.Optdef.to_option(Js.Unsafe.get(Js.Unsafe.global, "localStorage"))
  ) {
  | None => None
  | Some(ls) =>
    Js.Opt.to_option(
      Js.Unsafe.meth_call(
        ls,
        "getItem",
        [|Js.Unsafe.inject(Js.string(k))|],
      ),
    )
    |> Option.map(Js.to_string)
  };
let storage_set = (k: string, v: string): unit =>
  switch (
    Js.Optdef.to_option(Js.Unsafe.get(Js.Unsafe.global, "localStorage"))
  ) {
  | None => ()
  | Some(ls) =>
    ignore(
      Js.Unsafe.meth_call(
        ls,
        "setItem",
        [|
          Js.Unsafe.inject(Js.string(k)),
          Js.Unsafe.inject(Js.string(v)),
        |],
      ),
    )
  };

/* "glyph" (the @ box) or "rig" */
let look: ref(string) =
  ref(
    switch (storage_get(look_key)) {
    | Some("rig") => "rig"
    | _ => "glyph"
    },
  );
let is_rig = (): bool => look^ == "rig";
let toggle_look = (): unit => {
  look := is_rig() ? "glyph" : "rig";
  storage_set(look_key, look^);
  CanvasLog.log("avatar: look -> " ++ look^);
};

/* ---- the body element and its mood ---- */

let body = (): option(Js.t(Dom_html.element)) =>
  Util.JsUtil.get_elem_by_id_opt("canvas-avatar")
  |> Util.OptUtil.and_then(el =>
       Js.Opt.to_option(Js.Unsafe.get(el, "firstElementChild"))
     );

let moods = ["travel", "arrive", "edit", "draw", "erase", "tidy", "think"];
let current_mood: ref(string) = ref("");

let set_mood = (m: string): unit => {
  current_mood := m;
  switch (body()) {
  | None => ()
  | Some(b) =>
    let cl = Js.Unsafe.get(b, "classList");
    List.iter(
      x =>
        ignore(
          Js.Unsafe.meth_call(
            cl,
            "remove",
            [|Js.Unsafe.inject(Js.string("mood-" ++ x))|],
          ),
        ),
      moods,
    );
    if (m != "") {
      ignore(
        Js.Unsafe.meth_call(
          cl,
          "add",
          [|Js.Unsafe.inject(Js.string("mood-" ++ m))|],
        ),
      );
    };
  };
};

/* a heading hint from the score (degrees): used until the body actually
   moves, then its own motion steers it */
let heading_hint: ref(option(float)) = ref(None);
let set_heading = (deg: float): unit =>
  heading_hint := Some(deg *. Float.pi /. 180.);
let heading_of =
    ((x0, y0): (float, float), (x1, y1): (float, float)): float =>
  atan2(y1 -. y0, x1 -. x0) *. 180. /. Float.pi;

/* how far the segment about to be traveled goes (board px): the beam
   reaches further for a long trip, as the mockup's did */
let travel_len: ref(float) = ref(0.);
let set_travel_len = (len: float): unit => travel_len := len;
/* the beam fades in and out over a few frames (the demo's beamAlpha) */
let beam_alpha: ref(float) = ref(0.);
let last_thinking: ref(bool) = ref(false);

/* ---- the rig: three vertices on springs, driven every frame ----
   The mockup's body (plans/agent-canvas-mockups/avatar-concepts-4.html):
   targets on a breathing circle, each vertex a spring toward its target
   (k 170, damping 0.95 critical); traveling turns the triangle so vertex 0
   leads, pulls it ahead of the others (stiff leader, soft followers) and
   squashes on arrival; thinking spins; error jitters and drops an edge.
   The body's own screen motion (the WAAPI ride) gives heading and speed,
   so the beam always points where the body is actually going. */

type vtx = {
  mutable x: float,
  mutable y: float,
  mutable vx: float,
  mutable vy: float,
};

type rig = {
  v: array(vtx),
  mutable t: float,
  mutable rot: float,
  mutable heading: float,
  mutable speed: float, /* board px/s, smoothed */
  mutable aspect: float,
  mutable aspect_v: float,
  mutable was_traveling: bool,
  mutable last_pos: option((float, float)),
  mutable last_ms: float,
  mutable emoji_next: array(float),
  mutable emoji_cur: array(string),
  mutable running: bool,
};

let tau = 2. *. Float.pi;
let base_r = 11.; /* the triangle spans 22 px; a type node is 40+ */

let rig: rig = {
  v:
    Array.init(
      3,
      i => {
        let a = -. Float.pi /. 2. +. float_of_int(i) *. tau /. 3.;
        {
          x: cos(a) *. base_r,
          y: sin(a) *. base_r,
          vx: 0.,
          vy: 0.,
        };
      },
    ),
  t: Random.float(10.),
  rot: 0.,
  heading: -. Float.pi /. 2.,
  speed: 0.,
  aspect: 1.,
  aspect_v: 0.,
  was_traveling: false,
  last_pos: None,
  last_ms: 0.,
  emoji_next: [|0., 0., 0.|],
  emoji_cur: [|{js|🍄|js}, {js|📦|js}, {js|🧩|js}|],
  running: false,
};

let emoji = [|
  {js|🍄|js},
  {js|📦|js},
  {js|🧩|js},
  {js|🎯|js},
  {js|🐚|js},
  {js|🌰|js},
  {js|🍋|js},
  {js|🥑|js},
  {js|🧀|js},
  {js|🍩|js},
  {js|🐙|js},
  {js|🦀|js},
  {js|🐌|js},
  {js|🐢|js},
  {js|🦉|js},
  {js|🌵|js},
  {js|🌻|js},
  {js|💎|js},
  {js|🔮|js},
  {js|🧸|js},
  {js|🎈|js},
  {js|🎲|js},
  {js|🧲|js},
  {js|🔑|js},
  {js|🪐|js},
  {js|🌙|js},
  {js|⭐|js},
  {js|🐞|js},
  {js|🐝|js},
  {js|🦋|js},
  {js|🍀|js},
|];

let anchor_has = (cls: string): bool =>
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-avatar")) {
  | Some(el) =>
    Js.to_bool(
      Js.Unsafe.meth_call(
        Js.Unsafe.get(el, "classList"),
        "contains",
        [|Js.Unsafe.inject(Js.string(cls))|],
      ),
    )
  | None => false
  };

let lerp = (a, b, t) => a +. (b -. a) *. t;
let set = (el, name: string, v: string): unit =>
  ignore(
    Js.Unsafe.meth_call(
      el,
      "setAttribute",
      [|
        Js.Unsafe.inject(Js.string(name)),
        Js.Unsafe.inject(Js.string(v)),
      |],
    ),
  );
let f1 = (x: float): string => Printf.sprintf("%.1f", x);
let q = (b, sel: string) =>
  Js.Unsafe.meth_call(
    b,
    "querySelectorAll",
    [|Js.Unsafe.inject(Js.string(sel))|],
  );
let item = (nodes, i: int) =>
  Js.Unsafe.meth_call(nodes, "item", [|Js.Unsafe.inject(i)|]);

/* write the rig's current geometry into a root holding .rig-edge /
   .rig-dot / .rig-disc / .rig-emo triples (the body's svg, or a mirror) */
let draw_rig = (root, ~thinking: bool, ~err: bool): unit => {
  let edges = q(root, ".rig-edge")
  and dots = q(root, ".rig-dot")
  and discs = q(root, ".rig-disc")
  and texts = q(root, ".rig-emo");
  let n: int = Js.Unsafe.get(edges, "length");
  if (n >= 3) {
    let hide_edge = err ? int_of_float(rig.t *. 6.) mod 3 : (-1);
    for (i in 0 to 2) {
      let a = rig.v[i]
      and c = rig.v[(i + 1) mod 3];
      let e = item(edges, i);
      set(e, "x1", f1(a.x));
      set(e, "y1", f1(a.y));
      set(e, "x2", f1(c.x));
      set(e, "y2", f1(c.y));
      set(e, "opacity", hide_edge == i ? "0.08" : "0.9");
      let d = item(dots, i);
      set(d, "cx", f1(a.x));
      set(d, "cy", f1(a.y));
      set(d, "opacity", thinking ? "0" : "1");
      let disc = item(discs, i);
      set(disc, "cx", f1(a.x));
      set(disc, "cy", f1(a.y));
      set(disc, "opacity", thinking ? "1" : "0");
      let t = item(texts, i);
      set(t, "x", f1(a.x));
      set(t, "y", f1(a.y));
      set(t, "opacity", thinking ? "1" : "0");
      if (thinking) {
        Js.Unsafe.set(t, "textContent", Js.string(rig.emoji_cur[i]));
      };
    };
  };
};

let last_mirror = () => {
  let all =
    Js.Unsafe.meth_call(
      Js.Unsafe.global##.document,
      "querySelectorAll",
      [|Js.Unsafe.inject(Js.string(".rig-mirror"))|],
    );
  let n: int = Js.Unsafe.get(all, "length");
  n == 0 ? None : Some(item(all, n - 1));
};

/* one frame: read the body's motion, move the targets, spring, draw */
let step = (b, now_ms: float): unit => {
  let dt = min(0.05, max(0.001, (now_ms -. rig.last_ms) /. 1000.));
  rig.last_ms = now_ms;
  rig.t = rig.t +. dt;
  /* where the body is on screen (the ride's transform included), in
     board units so speed does not depend on the zoom */
  let zoom = max(0.2, CanvasBuffer.canvas_zoom^);
  let rect = Js.Unsafe.meth_call(b, "getBoundingClientRect", [||]);
  let cx: float = Js.Unsafe.get(rect, "left") /. zoom
  and cy: float = Js.Unsafe.get(rect, "top") /. zoom;
  let (mdx, mdy) =
    switch (rig.last_pos) {
    | Some((px, py)) => (cx -. px, cy -. py)
    | None => (0., 0.)
    };
  rig.last_pos = Some((cx, cy));
  let inst = Float.hypot(mdx, mdy) /. dt;
  rig.speed = lerp(rig.speed, min(inst, 600.), min(1., dt *. 10.));
  let moving = rig.speed > 25.;
  if (Float.hypot(mdx, mdy) > 0.6) {
    let want = atan2(mdy, mdx);
    let da = ref(want -. rig.heading);
    while (da^ > Float.pi) {
      da := da^ -. tau;
    };
    while (da^ < -. Float.pi) {
      da := da^ +. tau;
    };
    rig.heading = rig.heading +. da^ *. min(1., dt *. 8.);
  } else {
    switch (heading_hint^) {
    | Some(h) when !moving =>
      let da = ref(h -. rig.heading);
      while (da^ > Float.pi) {
        da := da^ -. tau;
      };
      while (da^ < -. Float.pi) {
        da := da^ +. tau;
      };
      rig.heading = rig.heading +. da^ *. min(1., dt *. 4.);
    | _ => ()
    };
  };
  let mood = current_mood^;
  /* the state class is a render-time reading (stale through a score with
     few renders): read the score live, and never think while moving —
     the depicted timeline is the score's, so an enacted edit is editing
     even if the model is literally already thinking about the next step */
  let score_on = CanvasBuffer.score_playing();
  let thinking0 = anchor_has("avatar-think") || mood == "think";
  let err = anchor_has("avatar-err");
  let pen = mood == "draw" || mood == "erase";
  let traveling = moving || pen && rig.speed > 8.;
  let thinking = thinking0 && !score_on && !traveling && !pen;
  last_thinking := thinking;
  let editing =
    (mood == "edit" || anchor_has("avatar-edit") && score_on)
    && !moving
    && !pen;
  if (!traveling) {
    /* the trip is over: the beam's reach eases back */
    travel_len := travel_len^ *. (1. -. min(1., dt *. 4.));
  };
  /* arrival: squash along the heading, then spring back */
  if (rig.was_traveling && !traveling) {
    rig.aspect = 0.72;
    rig.aspect_v = 0.;
  };
  rig.was_traveling = traveling;
  rig.aspect_v =
    rig.aspect_v
    +. (-. (rig.aspect -. 1.) *. 170. -. rig.aspect_v *. 12.)
    *. dt;
  rig.aspect = rig.aspect +. rig.aspect_v *. dt;
  /* breathing with a slowly drifting amplitude; never quite still */
  let amp = 0.12 *. (0.75 +. 0.25 *. sin(rig.t *. 0.37));
  let breathe = 1. +. amp *. sin(rig.t *. 2.0);
  let r = ref(base_r *. breathe);
  let k = ref(170.)
  and k_follow = ref(170.);
  let jitter = ref(0.);
  let stretch = min(1., rig.speed /. 220.);
  /* spin by state */
  if (traveling) {
    let head_rot = rig.heading +. Float.pi /. 2.;
    let da = ref(head_rot -. rig.rot);
    while (da^ > Float.pi) {
      da := da^ -. tau;
    };
    while (da^ < -. Float.pi) {
      da := da^ +. tau;
    };
    rig.rot = rig.rot +. da^ *. min(1., dt *. 6.);
    k := 240.;
    k_follow := 55.;
  } else if (thinking) {
    rig.rot = rig.rot +. dt *. 1.6 *. (1. +. 0.25 *. sin(rig.t *. 0.7));
    r := r^ *. 1.15;
  } else if (editing) {
    rig.rot = rig.rot +. dt *. 0.5;
    r := r^ *. 1.35;
  } else {
    /* idle: a slow turn, so it is never a still picture */
    rig.rot =
      rig.rot +. dt *. 0.18;
  };
  if (err) {
    jitter := 3.;
    r := r^ *. 0.95;
  };
  /* the rig's center: on the node while editing (the anchor hangs
     above-right of the site by (14, -34)), a gentle bob otherwise */
  let (ox, oy) = editing ? ((-14.), 34.) : (0., 1.4 *. sin(rig.t *. 2.4));
  let hc = cos(rig.heading +. Float.pi /. 2.)
  and hs = sin(rig.heading +. Float.pi /. 2.);
  for (i in 0 to 2) {
    let a = -. Float.pi /. 2. +. float_of_int(i) *. tau /. 3. +. rig.rot;
    let lx = cos(a) *. r^
    and ly = sin(a) *. r^;
    /* squash along the heading frame */
    let ax = lx *. hc +. ly *. hs
    and ay = (-. lx *. hs +. ly *. hc) *. (traveling ? 1. : rig.aspect);
    let tx = ref(ax *. hc -. ay *. hs +. ox)
    and ty = ref(ax *. hs +. ay *. hc +. oy);
    if (traveling && i == 0) {
      let ahead = 16. *. stretch;
      tx := tx^ +. cos(rig.heading) *. ahead;
      ty := ty^ +. sin(rig.heading) *. ahead;
    };
    let n = rig.v[i];
    let kk = i == 0 ? k^ : k_follow^;
    let dd = 2. *. sqrt(kk) *. 0.95;
    n.vx = n.vx +. ((tx^ -. n.x) *. kk -. n.vx *. dd) *. dt;
    n.vy = n.vy +. ((ty^ -. n.y) *. kk -. n.vy *. dd) *. dt;
    n.x = n.x +. n.vx *. dt;
    n.y = n.y +. n.vy *. dt;
    if (jitter^ > 0.) {
      n.x = n.x +. (Random.float(1.) -. 0.5) *. jitter^ *. 2.;
      n.y = n.y +. (Random.float(1.) -. 0.5) *. jitter^ *. 2.;
    };
  };
  /* thinking: emoji on the vertices, each on its own irregular clock */
  if (thinking) {
    for (i in 0 to 2) {
      if (rig.t >= rig.emoji_next[i]) {
        rig.emoji_cur[i] = emoji[Random.int(Array.length(emoji))];
        rig.emoji_next[i] = rig.t +. 0.25 +. Random.float(0.6);
      };
    };
  };
  draw_rig(b, ~thinking, ~err);
  /* the chat's brand icon mirrors the live rig (the last one in the
     chat; earlier ones keep the state they froze in) */
  switch (last_mirror()) {
  | Some(m) => draw_rig(m, ~thinking, ~err)
  | None => ()
  };
  /* the beam: the demo's — a sector of spread 0.36 and length 150 at R 18
     (so 8.3 R), filled by a radial gradient from the leading vertex
     (#ffe58a 0.55 -> 0), eased in and out; the glow disc at the vertex */
  let lead = rig.v[0];
  let beam = item(q(b, ".rig-beam"), 0)
  and glow = item(q(b, ".rig-glow"), 0)
  and grad = item(q(b, ".rig-beam-grad"), 0);
  let want = traveling && !pen ? 1. : 0.;
  beam_alpha := beam_alpha^ +. (want -. beam_alpha^) *. min(1., dt *. 8.);
  let reach = base_r *. 150. /. 18.;
  let h = rig.heading
  and spread = 0.36;
  set(
    beam,
    "d",
    Printf.sprintf(
      "M %s %s L %s %s A %s %s 0 0 1 %s %s Z",
      f1(lead.x),
      f1(lead.y),
      f1(lead.x +. cos(h -. spread) *. reach),
      f1(lead.y +. sin(h -. spread) *. reach),
      f1(reach),
      f1(reach),
      f1(lead.x +. cos(h +. spread) *. reach),
      f1(lead.y +. sin(h +. spread) *. reach),
    ),
  );
  set(beam, "opacity", f1(beam_alpha^));
  set(grad, "cx", f1(lead.x));
  set(grad, "cy", f1(lead.y));
  set(grad, "r", f1(reach));
  set(glow, "cx", f1(lead.x));
  set(glow, "cy", f1(lead.y));
  set(glow, "opacity", f1(0.5 *. max(beam_alpha^, pen ? 1. : 0.)));
};

let rec loop = (now_ms: float): unit =>
  if (!is_rig()) {
    rig.running = false;
  } else {
    switch (body()) {
    | Some(b) =>
      /* a long stall would make the springs explode: the dt clamp in
         step handles it, but a fresh dt anchor is safer */
      if (now_ms -. rig.last_ms > 500.) {
        rig.last_ms = now_ms -. 16.;
      };
      step(b, now_ms);
    | None => ()
    };
    ignore(
      Js.Unsafe.global##requestAnimationFrame(Js.Unsafe.callback(loop)),
    );
  };

let ensure_loop = (): unit =>
  if (!rig.running) {
    rig.running = true;
    rig.last_ms = Js.Unsafe.global##.performance##now() -. 16.;
    ignore(
      Js.Unsafe.global##requestAnimationFrame(Js.Unsafe.callback(loop)),
    );
  };

/* ---- the rig (vdom): static structure only; every animated attribute
   is written by the loop, and the vdom never declares those, so a
   re-render cannot fight it ---- */

let svg = (tag, attrs, kids) => Node.create_svg(tag, ~attrs, kids);

let rig_view = (): Node.t => {
  ensure_loop();
  let three = f => List.init(3, f);
  Node.div(
    ~attrs=[Attr.classes(["rig-box"])],
    [
      svg(
        "svg",
        [
          Attr.classes(["rig"]),
          Attr.create("viewBox", "-40 -40 80 80"),
          Attr.create("width", "80"),
          Attr.create("height", "80"),
        ],
        [
          svg(
            "defs",
            [],
            [
              svg(
                "radialGradient",
                [
                  Attr.id("rig-beam-grad"),
                  Attr.classes(["rig-beam-grad"]),
                  Attr.create("gradientUnits", "userSpaceOnUse"),
                ],
                [
                  svg(
                    "stop",
                    [
                      Attr.create("offset", "0"),
                      Attr.create("stop-color", "#ffe58a"),
                      Attr.create("stop-opacity", "0.55"),
                    ],
                    [],
                  ),
                  svg(
                    "stop",
                    [
                      Attr.create("offset", "1"),
                      Attr.create("stop-color", "#ffe58a"),
                      Attr.create("stop-opacity", "0"),
                    ],
                    [],
                  ),
                ],
              ),
            ],
          ),
          svg(
            "path",
            [
              Attr.classes(["rig-beam"]),
              Attr.create("fill", "url(#rig-beam-grad)"),
            ],
            [],
          ),
          svg(
            "circle",
            [Attr.classes(["rig-glow"]), Attr.create("r", "6")],
            [],
          ),
        ]
        @ three(_ => svg("line", [Attr.classes(["rig-edge"])], []))
        @ three(_ =>
            svg(
              "circle",
              [Attr.classes(["rig-disc"]), Attr.create("r", "6.5")],
              [],
            )
          )
        @ three(i =>
            svg(
              "circle",
              [
                Attr.classes(["rig-dot", "rig-v" ++ string_of_int(i)]),
                Attr.create("r", "3.6"),
              ],
              [],
            )
          )
        @ three(_ =>
            svg(
              "text",
              [
                Attr.classes(["rig-emo"]),
                Attr.create("text-anchor", "middle"),
                Attr.create("dominant-baseline", "central"),
              ],
              [],
            )
          ),
      ),
    ],
  );
};

/* the circle that contains everything the avatar draws, in BODY-LOCAL px
   (the rig's coordinates; for the glyph, the chip's box): bubbles anchor
   on it, never inside it */
let bounding_circle = (): (float, float, float) =>
  if (is_rig()) {
    let n = float_of_int(Array.length(rig.v));
    let cx = Array.fold_left((a, v) => a +. v.x, 0., rig.v) /. n
    and cy = Array.fold_left((a, v) => a +. v.y, 0., rig.v) /. n;
    let reach =
      Array.fold_left(
        (m, v) => max(m, Float.hypot(v.x -. cx, v.y -. cy)),
        0.,
        rig.v,
      );
    (cx, cy, reach +. (last_thinking^ ? 6.7 : 3.6) +. 2.);
  } else {
    switch (body()) {
    | Some(b) =>
      let w: float = Js.Unsafe.get(b, "offsetWidth")
      and h: float = Js.Unsafe.get(b, "offsetHeight");
      (w /. 2., h /. 2., Float.hypot(w, h) /. 2. +. 1.);
    | None => (0., 0., 12.)
    };
  };

/* the chat's brand icon: a mirror of the rig (live for the last one on
   the page), or the glyph chip — continuity between the two views */
let brand_icon = (): Node.t =>
  if (is_rig()) {
    let three = f => List.init(3, f);
    Node.create_svg(
      "svg",
      ~attrs=[
        Attr.classes(["rig-mirror"]),
        Attr.create("viewBox", "-24 -24 48 48"),
        Attr.create("width", "22"),
        Attr.create("height", "22"),
      ],
      three(_ => svg("line", [Attr.classes(["rig-edge"])], []))
      @ three(_ =>
          svg(
            "circle",
            [Attr.classes(["rig-disc"]), Attr.create("r", "6.7")],
            [],
          )
        )
      @ three(_ =>
          svg(
            "circle",
            [Attr.classes(["rig-dot"]), Attr.create("r", "3.2")],
            [],
          )
        )
      @ three(_ =>
          svg(
            "text",
            [
              Attr.classes(["rig-emo"]),
              Attr.create("text-anchor", "middle"),
              Attr.create("dominant-baseline", "central"),
            ],
            [],
          )
        ),
    );
  } else {
    Node.span(~attrs=[Attr.classes(["brand-glyph"])], [Node.text("@")]);
  };
