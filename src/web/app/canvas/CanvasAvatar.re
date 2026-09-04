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

/* the heading the leading vertex faces, in degrees; the CSS transition
   turns the rig, so consecutive headings read as steering */
let last_heading: ref(float) = ref(0.);
let set_heading = (deg: float): unit => {
  /* take the short way round */
  let d = ref(deg);
  while (d^ -. last_heading^ > 180.) {
    d := d^ -. 360.;
  };
  while (d^ -. last_heading^ < (-180.)) {
    d := d^ +. 360.;
  };
  last_heading := d^;
  switch (body()) {
  | None => ()
  | Some(b) =>
    ignore(
      Js.Unsafe.meth_call(
        Js.Unsafe.get(b, "style"),
        "setProperty",
        [|
          Js.Unsafe.inject(Js.string("--heading")),
          Js.Unsafe.inject(Js.string(Printf.sprintf("%.0fdeg", d^))),
        |],
      ),
    )
  };
};

let heading_of =
    ((x0, y0): (float, float), (x1, y1): (float, float)): float =>
  atan2(y1 -. y0, x1 -. x0) *. 180. /. Float.pi;

/* ---- thinking: emoji on the vertices, swapped at the pace of thought ---- */

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

let emoji_timer: ref(option(Js.Unsafe.any)) = ref(None);
let thinking = (): bool =>
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-avatar")) {
  | Some(el) =>
    let cl = Js.Unsafe.get(el, "classList");
    Js.to_bool(
      Js.Unsafe.meth_call(
        cl,
        "contains",
        [|Js.Unsafe.inject(Js.string("avatar-think"))|],
      ),
    )
    || current_mood^ == "think";
  | None => false
  };
let swap_one_emoji = (): unit =>
  switch (body()) {
  | None => ()
  | Some(b) =>
    let texts =
      Js.Unsafe.meth_call(
        b,
        "querySelectorAll",
        [|Js.Unsafe.inject(Js.string(".rig-emo"))|],
      );
    let n: int = Js.Unsafe.get(texts, "length");
    if (n > 0) {
      let i = Random.int(n);
      let t = Js.Unsafe.meth_call(texts, "item", [|Js.Unsafe.inject(i)|]);
      Js.Unsafe.set(
        t,
        "textContent",
        Js.string(emoji[Random.int(Array.length(emoji))]),
      );
    };
  };
let ensure_emoji_loop = (): unit =>
  if (emoji_timer^ == None) {
    emoji_timer :=
      Some(
        Js.Unsafe.global##setInterval(
          Js.Unsafe.callback(() =>
            if (is_rig() && thinking()) {
              swap_one_emoji();
            }
          ),
          1100.,
        ),
      );
  };

/* ---- the rig (vdom) ---- */

let r = 11.; /* circumradius: the triangle spans 22 px, a node 40+ */
let vertex = (i: int): (float, float) => {
  let a = float_of_int(i) *. 2. *. Float.pi /. 3.;
  (r *. cos(a), r *. sin(a));
};
let fmt = (f: float): string => Printf.sprintf("%.2f", f);
let svg = (tag, attrs, kids) => Node.create_svg(tag, ~attrs, kids);

let rig_view = (): Node.t => {
  ensure_emoji_loop();
  let (x0, y0) = vertex(0)
  and (x1, y1) = vertex(1)
  and (x2, y2) = vertex(2);
  let vtx = (i, (x, y)) =>
    svg(
      "g",
      [
        Attr.classes(["rig-vtx", "rig-v" ++ string_of_int(i)]),
        Attr.create(
          "transform",
          Printf.sprintf("translate(%s %s)", fmt(x), fmt(y)),
        ),
      ],
      [
        svg(
          "circle",
          [Attr.classes(["rig-dot"]), Attr.create("r", "4.2")],
          [],
        ),
        svg(
          "text",
          [
            Attr.classes(["rig-emo"]),
            Attr.create("text-anchor", "middle"),
            Attr.create("dominant-baseline", "central"),
          ],
          [],
        ),
      ],
    );
  Node.div(
    ~attrs=[Attr.classes(["rig-heading"])],
    [
      svg(
        "svg",
        [
          Attr.classes(["rig"]),
          Attr.create("viewBox", "-24 -24 48 48"),
          Attr.create("width", "48"),
          Attr.create("height", "48"),
        ],
        [
          /* the beam ahead of the leading vertex (heading = +x) */
          svg(
            "path",
            [
              Attr.classes(["rig-beam"]),
              Attr.create("d", "M 10 0 L 46 -12 L 46 12 Z"),
            ],
            [],
          ),
          svg(
            "g",
            [Attr.classes(["rig-spin"])],
            [
              svg(
                "path",
                [
                  Attr.classes(["rig-edges"]),
                  Attr.create(
                    "d",
                    Printf.sprintf(
                      "M %s %s L %s %s L %s %s Z",
                      fmt(x0),
                      fmt(y0),
                      fmt(x1),
                      fmt(y1),
                      fmt(x2),
                      fmt(y2),
                    ),
                  ),
                ],
                [],
              ),
              vtx(0, (x0, y0)),
              vtx(1, (x1, y1)),
              vtx(2, (x2, y2)),
            ],
          ),
          /* the glow disc on the leading vertex, in the heading frame */
          svg(
            "circle",
            [
              Attr.classes(["rig-glow"]),
              Attr.create("cx", fmt(r)),
              Attr.create("cy", "0"),
              Attr.create("r", "7"),
            ],
            [],
          ),
        ],
      ),
    ],
  );
};
