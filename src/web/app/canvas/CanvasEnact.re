/* CanvasEnact — choreography derived from the graph DIFF of an agent
   beat, so it is a projection of the structural edit whatever tool made
   it. A new function edge is enacted as the ƒ gesture: the avatar
   travels from the edge's domain to its codomain PULLING the arrow along
   (the stroke reveals under it), then steps back to the pill, which
   fades in. A new MULTI-ARGUMENT function mirrors the shift-click
   gesture first: the avatar visits each domain type, the product dot
   forms from them (formation lines draw in, the dot grows), and only
   then is the arrow pulled from the dot to the codomain.

   Runs on the rendered DOM one frame after the beat lands (Web
   Animations), and every animation ends at the identity, so nothing
   here fights the vdom. */

open Js_of_ocaml;

let travel_ms = 700.; /* domain -> codomain */
let settle_ms = 220.; /* codomain -> the pill */
let visit_ms = 240.; /* per domain type of a multi-arg function */
let form_ms = 320.; /* formation lines drawing into the product dot */
let lead_ms = float_of_int(CanvasBuffer.lead_ms);
let samples = 18;
/* more new edges than this in one beat = a rewrite, not a gesture */
let max_edges = 3;

/* one new edge to enact */
type new_edge = {
  ne_name: string,
  /* a NEW anonymous product as the edge's source: (its node key, its
     component node keys) — the multi-argument case */
  ne_product: option((string, list(string))),
};

let by_id = (id: string): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(
    Js.Unsafe.global##.document##getElementById(Js.string(id)),
  );

let str = (s: string): Js.Unsafe.any => Js.Unsafe.inject(Js.string(s));
let num = (f: float): Js.Unsafe.any => Js.Unsafe.inject(f);

let set_attr = (el, a: string, v: string): unit =>
  ignore(Js.Unsafe.meth_call(el, "setAttribute", [|str(a), str(v)|]));
let get_attr = (el, a: string): option(string) =>
  Js.Opt.to_option(Js.Unsafe.meth_call(el, "getAttribute", [|str(a)|]))
  |> Option.map(Js.to_string);
let cancel_anims = (el): unit => {
  let anims = Js.Unsafe.meth_call(el, "getAnimations", [||]);
  let len: int = Js.Unsafe.get(anims, "length");
  for (i in 0 to len - 1) {
    ignore(Js.Unsafe.meth_call(Js.Unsafe.get(anims, i), "cancel", [||]));
  };
};
let center_of = (el): (float, float) => {
  let r = Js.Unsafe.meth_call(el, "getBoundingClientRect", [||]);
  let x: float = Js.Unsafe.get(r, "left") +. Js.Unsafe.get(r, "width") /. 2.
  and y: float = Js.Unsafe.get(r, "top") +. Js.Unsafe.get(r, "height") /. 2.;
  (x, y);
};
let later = (ms: float, f: unit => unit): unit =>
  ignore(Js.Unsafe.global##setTimeout(Js.Unsafe.callback(f), ms));

/* the path's length and evenly spaced points along it, in SCREEN px */
let sample_path = (path): (float, list((float, float))) => {
  let total: float = Js.Unsafe.meth_call(path, "getTotalLength", [||]);
  let ctm = Js.Unsafe.meth_call(path, "getScreenCTM", [||]);
  let pts =
    List.init(
      samples + 1,
      i => {
        let pt =
          Js.Unsafe.meth_call(
            path,
            "getPointAtLength",
            [|num(total *. float_of_int(i) /. float_of_int(samples))|],
          );
        let sp =
          Js.Unsafe.meth_call(
            pt,
            "matrixTransform",
            [|Js.Unsafe.inject(ctm)|],
          );
        let x: float = Js.Unsafe.get(sp, "x")
        and y: float = Js.Unsafe.get(sp, "y");
        (x, y);
      },
    );
  (total, pts);
};

let animate =
    (
      el,
      frames: list(list((string, Js.Unsafe.any))),
      opts: list((string, Js.Unsafe.any)),
    )
    : unit => {
  let kf =
    frames
    |> List.map(props => Js.Unsafe.obj(Array.of_list(props)))
    |> Array.of_list
    |> Js.array;
  ignore(
    Js.Unsafe.meth_call(
      el,
      "animate",
      [|
        Js.Unsafe.inject(kf),
        Js.Unsafe.inject(Js.Unsafe.obj(Array.of_list(opts))),
      |],
    ),
  );
};

let clear_dash = (el): unit => {
  let st = Js.Unsafe.get(el, "style");
  Js.Unsafe.set(st, "strokeDasharray", Js.string(""));
  Js.Unsafe.set(st, "strokeDashoffset", Js.string(""));
  ignore(
    Js.Unsafe.meth_call(el, "removeAttribute", [|str("stroke-dasharray")|]),
  );
  ignore(
    Js.Unsafe.meth_call(
      el,
      "removeAttribute",
      [|str("stroke-dashoffset")|],
    ),
  );
};

/* one dash the length of the path, on the INLINE style (a dotted line's
   CSS pattern beats the attribute); dotted lines keep their dots. Returns
   the "on" length the offset animates from. */
let arm_dash = (el, total: float): float => {
  let css: string =
    Js.to_string(
      Js.Unsafe.get(
        Js.Unsafe.meth_call(
          Js.Unsafe.global##.window,
          "getComputedStyle",
          [|Js.Unsafe.inject(el)|],
        ),
        "strokeDasharray",
      ),
    );
  let strip_px = s => {
    let s = String.trim(s);
    let n = String.length(s);
    n > 2 && String.sub(s, n - 2, 2) == "px" ? String.sub(s, 0, n - 2) : s;
  };
  let nums =
    css
    |> String.split_on_char(',')
    |> List.concat_map(String.split_on_char(' '))
    |> List.filter_map(s => float_of_string_opt(strip_px(s)));
  let (pattern, on_len) =
    switch (nums) {
    | [dot, gap, ..._] when dot +. gap > 0. && dot +. gap < 40. =>
      let period = dot +. gap;
      let n = int_of_float(ceil(total /. period));
      let on_len = float_of_int(n) *. period;
      (
        String.concat(
          " ",
          List.init(n, _ => Printf.sprintf("%.2f %.2f", dot, gap)),
        )
        ++ Printf.sprintf(" 0 %.1f", on_len),
        on_len,
      );
    | _ => (Printf.sprintf("%.1f %.1f", total, total), total)
    };
  Js.Unsafe.set(
    Js.Unsafe.get(el, "style"),
    "strokeDasharray",
    Js.string(pattern),
  );
  on_len;
};

/* draw a stroke on from its start over dur ms starting at delay */
let reveal = (~delay: float, ~dur: float, el): unit => {
  cancel_anims(el);
  let total: float = Js.Unsafe.meth_call(el, "getTotalLength", [||]);
  let on_len = arm_dash(el, total);
  /* a stale dash array would truncate the path once it later stretches */
  later(delay +. dur +. 30., () => clear_dash(el));
  animate(
    el,
    [
      [("strokeDashoffset", num(on_len))],
      [("strokeDashoffset", num(0.))],
    ],
    [
      ("duration", num(dur)),
      ("delay", num(delay)),
      ("easing", str("cubic-bezier(0.65, 0, 0.35, 1)")),
      ("fill", str("backwards")),
    ],
  );
};

/* screen-space waypoint the avatar passes at a time (ms into the beat) */
type waypoint = ((float, float), float);

/* one edge, starting at t0: (visit the domain types, form the product
   dot,) reveal the stroke while the avatar rides it, let the pill in.
   Returns the avatar's waypoints and when the ride is over. */
let enact_edge =
    (~t0: float, ne: new_edge): option((list(waypoint), float)) =>
  switch (by_id(CanvasView.path_dom_id(ne.ne_name))) {
  | None => None
  | Some(path) =>
    /* the beat pass may already be revealing this path generically;
       the ride replaces it */
    cancel_anims(path);
    let (total, pts) = sample_path(path);
    if (total < 8.) {
      None;
    } else {
      /* ---- multi-arg prelude: visits + formation ---- */
      let prelude: ref(list(waypoint)) = ref([]);
      let t_edge =
        switch (ne.ne_product) {
        | Some((pk, parts)) =>
          switch (by_id(CanvasView.node_dom_id(pk))) {
          | None => t0
          | Some(dot) =>
            let part_els =
              List.filter_map(k => by_id(CanvasView.node_dom_id(k)), parts);
            let n = float_of_int(List.length(part_els));
            let t_visited = t0 +. visit_ms *. n;
            prelude :=
              List.mapi(
                (i, el) =>
                  (center_of(el), t0 +. visit_ms *. float_of_int(i + 1)),
                part_els,
              )
              @ [(center_of(dot), t_visited +. form_ms)];
            /* the dot grows as its lines arrive; the lines draw in from
               each domain type */
            cancel_anims(dot);
            animate(
              dot,
              [
                [("transform", str("scale(0)"))],
                [("transform", str("scale(1)"))],
              ],
              [
                ("duration", num(220.)),
                ("delay", num(t_visited +. form_ms -. 160.)),
                ("easing", str("cubic-bezier(0.34, 1.4, 0.64, 1)")),
                ("fill", str("backwards")),
              ],
            );
            List.iter(
              k =>
                switch (by_id(CanvasView.formation_dom_id(k, pk))) {
                | Some(line) => reveal(~delay=t_visited, ~dur=form_ms, line)
                | None => ()
                },
              parts,
            );
            t_visited +. form_ms +. 80.;
          }
        | None => t0
        };
      /* ---- the ride ---- */
      let marker = get_attr(path, "marker-end");
      switch (marker) {
      | Some(_) => set_attr(path, "marker-end", "none")
      | None => ()
      };
      let on_len = arm_dash(path, total);
      animate(
        path,
        [
          [("strokeDashoffset", num(on_len))],
          [("strokeDashoffset", num(0.))],
        ],
        [
          ("duration", num(travel_ms)),
          ("delay", num(t_edge)),
          ("easing", str("cubic-bezier(0.65, 0, 0.35, 1)")),
          ("fill", str("backwards")),
        ],
      );
      later(
        t_edge +. travel_ms,
        () => {
          switch (marker) {
          | Some(m) => set_attr(path, "marker-end", m)
          | None => ()
          };
          clear_dash(path);
        },
      );
      /* the pill appears once the arrow is drawn */
      switch (by_id(CanvasView.edge_dom_id(ne.ne_name))) {
      | Some(pill) =>
        cancel_anims(pill);
        animate(
          pill,
          [[("opacity", num(0.))], [("opacity", num(1.))]],
          [
            ("duration", num(settle_ms)),
            ("delay", num(t_edge +. travel_ms)),
            ("fill", str("backwards")),
          ],
        );
      | None => ()
      };
      let n = float_of_int(List.length(pts) - 1);
      let ride =
        List.mapi(
          (i, p) => (p, t_edge +. travel_ms *. float_of_int(i) /. n),
          pts,
        );
      Some((prelude^ @ ride, t_edge +. travel_ms));
    };
  };

/* screen -> board coords, via the zoomed root's box */
let to_board =
    (~zoom: float, (sx, sy): (float, float)): option((float, float)) =>
  switch (
    Js.Opt.to_option(
      Js.Unsafe.meth_call(
        Js.Unsafe.global##.document,
        "querySelector",
        [|str(".canvas-root")|],
      ),
    )
  ) {
  | None => None
  | Some(root) =>
    let r = Js.Unsafe.meth_call(root, "getBoundingClientRect", [||]);
    let left: float = Js.Unsafe.get(r, "left")
    and top: float = Js.Unsafe.get(r, "top");
    Some(((sx -. left) /. zoom, (sy -. top) /. zoom));
  };
/* the camera follows a waypoint when its time comes (dead zone applies) */
let camera_at = (~zoom: float, at: float, sp: (float, float)): unit =>
  later(at, () =>
    switch (CanvasCamera.pane(), to_board(~zoom, sp)) {
    | (Some((aw, ah)), Some(bp)) => CanvasCamera.follow(~aw, ~ah, bp)
    | _ => ()
    }
  );

/* board coords of a node element's center (its style is the layout) */
let board_center = (el): option((float, float)) => {
  let st = Js.Unsafe.get(el, "style");
  let px = (v: string) =>
    try(Some(float_of_string(String.sub(v, 0, String.length(v) - 2)))) {
    | _ => None
    };
  switch (
    px(Js.to_string(Js.Unsafe.get(st, "left"))),
    px(Js.to_string(Js.Unsafe.get(st, "top"))),
  ) {
  | (Some(x), Some(y)) => Some((x, y))
  | _ => None
  };
};

/* how long the avatar lingers on an arrival before heading to the next */
let visit_hold_ms = 140.;
/* more than this many arrivals is a rewrite, not a tour */
let max_tour = 12;

/* enact a whole beat, one frame after it renders: the avatar tours the
   new nodes as they bloom (splashing each), then performs each new
   edge's gesture, then steps back to wherever the beat put it */
let enact_beat = (~zoom: float, edges: list(new_edge)): unit => {
  let arrivals =
    Animation.last_arrivals^
    |> List.filter(((id, _)) =>
         String.length(id) >= 6 && String.sub(id, 0, 6) == "cnode-"
       )
    |> List.filter_map(((id, d)) =>
         by_id(id) |> Option.map(el => (el, float_of_int(d)))
       );
  Animation.last_arrivals := [];
  /* splashes land as each node blooms */
  List.iter(
    ((el, d)) =>
      switch (board_center(el)) {
      | Some(p) => later(d +. 60., () => CanvasRipple.splash(~amp=4., p))
      | None => ()
      },
    arrivals,
  );
  let tour: list(waypoint) =
    List.length(arrivals) > max_tour
      ? []
      : List.concat_map(
          ((el, d)) => {
            let c = center_of(el);
            [(c, d), (c, d +. visit_hold_ms)];
          },
          arrivals,
        );
  let t_after_tour =
    switch (List.rev(tour)) {
    | [(_, t), ..._] => t +. 200.
    | [] => lead_ms
    };
  /* edges, one after another, after the tour */
  let (edge_wps, t_end, done_) =
    if (List.length(edges) <= max_edges) {
      List.fold_left(
        ((wps, t, done_), ne) =>
          switch (enact_edge(~t0=t, ne)) {
          | Some((w, t')) => (wps @ w, t', [ne, ...done_])
          | None => (wps, t, done_)
          },
        ([], t_after_tour, []),
        edges,
      );
    } else {
      ([], t_after_tour, []);
    };
  let waypoints = tour @ edge_wps;
  /* the camera keeps the avatar in view along the whole timeline: each
     visited node as it blooms, each edge's far end as the ride starts */
  List.iter(
    ((c, d)) => camera_at(~zoom, d, c),
    List.filteri((i, _) => i mod 2 == 0, tour),
  );
  ignore(
    List.fold_left(
      (t, ne) => {
        switch (by_id(CanvasView.path_dom_id(ne.ne_name))) {
        | Some(path) =>
          let (_, pts) = sample_path(path);
          switch (List.rev(pts)) {
          | [far, ..._] => camera_at(~zoom, t, far)
          | [] => ()
          };
        | None => ()
        };
        t;
      },
      t_after_tour,
      List.rev(done_),
    ),
  );
  /* the avatar: from where it was, through every waypoint, back to the
     spot the beat gave it (its own FLIP hop is replaced) */
  switch (by_id(CanvasView.avatar_dom_id)) {
  | Some(av) when waypoints != [] =>
    let (ax, ay) = center_of(av);
    let total_ms = t_end +. settle_ms;
    let frame = ((x, y), at) => [
      (
        "transform",
        str(
          Printf.sprintf(
            "translate(%.1fpx, %.1fpx)",
            (x -. ax) /. zoom,
            (y -. ay) /. zoom,
          ),
        ),
      ),
      ("offset", num(max(0., min(1., at /. total_ms)))),
    ];
    let start =
      switch (CanvasBuffer.avatar_prev^) {
      | Some(p) => p
      | None => fst(List.hd(waypoints))
      };
    let frames =
      [frame(start, 0.)]
      @ List.map(((p, at)) => frame(p, at), waypoints)
      @ [
        [("transform", str("translate(0px, 0px)")), ("offset", num(1.))],
      ];
    /* per-keyframe easing: one ease over the whole timeline made the
       first travel crawl and the middle rush */
    let frames =
      List.map(kf => kf @ [("easing", str("ease-in-out"))], frames);
    {
      let anims = Js.Unsafe.meth_call(av, "getAnimations", [||]);
      let n: int = Js.Unsafe.get(anims, "length");
      if (n > 0) {
        CanvasLog.log(
          Printf.sprintf(
            "avatar: %d running animation(s) replaced by the new score",
            n,
          ),
        );
      };
    };
    cancel_anims(av);
    animate(
      av,
      frames,
      [("duration", num(total_ms)), ("easing", str("linear"))],
    );
  | _ => ()
  };
  if (arrivals != [] || done_ != []) {
    CanvasLog.log(
      Printf.sprintf(
        "enact: tour of %d arrival(s)%s",
        List.length(arrivals),
        done_ == []
          ? ""
          : Printf.sprintf(
              ", %d edge(s) drawn (%s)",
              List.length(done_),
              String.concat(
                ", ",
                List.rev_map(
                  ne =>
                    ne.ne_name
                    ++ (
                      switch (ne.ne_product) {
                      | Some((_, parts)) =>
                        Printf.sprintf(
                          " via %d-ary product",
                          List.length(parts),
                        )
                      | None => ""
                      }
                    ),
                  done_,
                ),
              ),
            ),
      ),
    );
  };
};

/* edges only (testers): the beat's arrivals, if any, are still toured */
let enact_edges = (~zoom: float, edges: list(new_edge)): unit =>
  enact_beat(~zoom, edges);

/* ======================= the score player =======================
   CanvasScore decides WHAT happens WHEN; this only maps its acts onto
   the rendered DOM (Web Animations), one frame after the beat lands.
   Node/pill arrivals are Animation's (fed the score's times before the
   render); rides, formations, ripples, the camera and the avatar's own
   path are scheduled here from the same score. */

/* ---- board-space geometry: the player never measures the screen. Node
   centers come from their layout style, path points from the SVG's own
   user space (board units), so a camera scroll or zoom mid-score cannot
   bend the avatar's path (a CSS-zoomed root scales transforms too). ---- */
let node_board = (k: string): option((float, float)) =>
  by_id(CanvasView.node_dom_id(k)) |> Util.OptUtil.and_then(board_center);

let mean = (ps: list((float, float))): option((float, float)) =>
  switch (ps) {
  | [] => None
  | _ =>
    let n = float_of_int(List.length(ps));
    Some((
      List.fold_left((s, (x, _)) => s +. x, 0., ps) /. n,
      List.fold_left((s, (_, y)) => s +. y, 0., ps) /. n,
    ));
  };

/* the path's length and evenly spaced points along it, in BOARD units */
let sample_path_board = (path): (float, list((float, float))) => {
  let total: float = Js.Unsafe.meth_call(path, "getTotalLength", [||]);
  let pts =
    List.init(
      samples + 1,
      i => {
        let pt =
          Js.Unsafe.meth_call(
            path,
            "getPointAtLength",
            [|num(total *. float_of_int(i) /. float_of_int(samples))|],
          );
        let x: float = Js.Unsafe.get(pt, "x")
        and y: float = Js.Unsafe.get(pt, "y");
        (x, y);
      },
    );
  (total, pts);
};

/* the arrow pulled along its path from t for dur ms; returns the
   avatar's waypoints along the ride */
let ride = (~t: float, ~dur: float, name: string): list(waypoint) =>
  switch (by_id(CanvasView.path_dom_id(name))) {
  | None => []
  | Some(path) =>
    cancel_anims(path);
    let (total, pts) = sample_path_board(path);
    if (total < 8.) {
      [];
    } else {
      /* the beat pass may have hidden this path already (marker stashed) */
      let marker =
        switch (get_attr(path, "data-marker"), get_attr(path, "marker-end")) {
        | (Some(m), _) => Some(m)
        | (None, Some("none")) => None
        | (None, m) => m
        };
      switch (get_attr(path, "marker-end")) {
      | Some(m) when m != "none" => set_attr(path, "marker-end", "none")
      | _ => ()
      };
      let on_len = arm_dash(path, total);
      animate(
        path,
        [
          [("strokeDashoffset", num(on_len))],
          [("strokeDashoffset", num(0.))],
        ],
        [
          ("duration", num(dur)),
          ("delay", num(t)),
          ("easing", str("cubic-bezier(0.65, 0, 0.35, 1)")),
          ("fill", str("backwards")),
        ],
      );
      later(
        t +. dur,
        () => {
          switch (marker) {
          | Some(m) => set_attr(path, "marker-end", m)
          | None => ()
          };
          clear_dash(path);
        },
      );
      let n = float_of_int(List.length(pts) - 1);
      List.mapi((i, p) => (p, t +. dur *. float_of_int(i) /. n), pts);
    };
  };

/* a product dot forming from its parts: the avatar visits each part,
   the lines draw into the dot, the dot grows */
let formation =
    (~t: float, ~dur: float, pk: string, parts: list(string))
    : list(waypoint) =>
  switch (by_id(CanvasView.node_dom_id(pk))) {
  | None => []
  | Some(dot) =>
    let part_pts = List.filter_map(node_board, parts);
    let n = List.length(part_pts);
    let visit = n > 0 ? (dur -. form_ms) /. float_of_int(n) : 0.;
    let t_visited = t +. visit *. float_of_int(n);
    cancel_anims(dot);
    animate(
      dot,
      [
        [("transform", str("scale(0)"))],
        [("transform", str("scale(1)"))],
      ],
      [
        ("duration", num(form_ms)),
        ("delay", num(t_visited)),
        ("easing", str("cubic-bezier(0.34, 1.4, 0.64, 1)")),
        ("fill", str("backwards")),
      ],
    );
    List.iter(
      k =>
        switch (by_id(CanvasView.formation_dom_id(k, pk))) {
        | Some(line) => reveal(~delay=t_visited, ~dur=form_ms, line)
        | None => ()
        },
      parts,
    );
    List.mapi((i, p) => (p, t +. visit *. float_of_int(i + 1)), part_pts)
    @ (
      switch (board_center(dot)) {
      | Some(c) => [(c, t_visited +. 120.)]
      | None => []
      }
    );
  };

let play = (~zoom: float, s: CanvasScore.score): unit => {
  ignore(zoom);
  /* the score owns every canvas element's motion until it ends: editor
     actions re-request FLIPs for all of them on each render, which would
     replace pending grow-ins and the avatar's path */
  Animation.hold(
    ~prefixes=["cnode-", "cedge-", "cval-", CanvasView.avatar_dom_id],
    ~until_ms=CanvasBuffer.now() +. float_of_int(s.total_ms) +. 300.,
  );
  let wps: ref(list(waypoint)) = ref([]);
  let add = (p, t) => wps := [(p, t), ...wps^];
  let path_point = (name, pick) =>
    by_id(CanvasView.path_dom_id(name))
    |> Util.OptUtil.and_then(path => {
         let (total, pts) = sample_path_board(path);
         total < 1. ? None : Some(pick(pts));
       });
  let site_board = (site: CanvasScore.site): option((float, float)) =>
    switch (site) {
    | Node(k) => node_board(k)
    | Centroid(ks) => mean(List.filter_map(node_board, ks))
    | Point(p) => Some((p.x, p.y))
    | Edge(name) => path_point(name, List.hd)
    | Here => None
    };
  /* an edge act ends where the ride ends (the codomain) */
  let site_end = (site: CanvasScore.site): option((float, float)) =>
    switch (site) {
    | Edge(name) =>
      path_point(name, pts => List.nth(pts, List.length(pts) - 1))
    | site => site_board(site)
    };
  /* the actor: arrive after travel, hold through the effects */
  List.iter(
    ((t, a): (int, CanvasScore.act)) =>
      switch (site_board(a.at), site_end(a.at)) {
      | (Some(sp), Some(ep)) =>
        add(sp, float_of_int(t + a.travel_ms));
        add(ep, float_of_int(t + CanvasScore.act_len(a) - a.settle_ms));
      | _ => ()
      },
    s.acts,
  );
  /* the effects */
  let vanish = ref(0);
  List.iter(
    ((t, a, e): (int, CanvasScore.act, CanvasScore.timed_effect)) => {
      let tf = float_of_int(t);
      switch (e.effect) {
      | Ripple(site) =>
        let erase = a.emote == CanvasScore.Erase;
        later(tf, () =>
          switch (site_board(site)) {
          | Some(bp) =>
            erase
              ? CanvasRipple.suction(bp) : CanvasRipple.splash(~amp=4., bp)
          | None => ()
          }
        );
      | Draw(name) =>
        List.iter(
          ((p, t)) => add(p, t),
          ride(~t=tf, ~dur=float_of_int(e.dur), name),
        )
      | Form(pk, parts) =>
        List.iter(
          ((p, t)) => add(p, t),
          formation(~t=tf, ~dur=float_of_int(e.dur), pk, parts),
        )
      | Vanish(_)
      | Erase(_) => incr(vanish)
      | Appear(_)
      | Pill(_)
      | Change(_)
      | Drift
      | Say(_) => ()
      };
    },
    CanvasScore.effects_abs(s),
  );
  if (vanish^ > 0) {
    CanvasLog.log(
      Printf.sprintf("vanish: %d removal(s) not yet choreographed", vanish^),
    );
  };
  /* the camera: the score's frames, each leading its act's travel */
  List.iter(
    ((t, f): (int, CanvasScore.frame)) =>
      later(float_of_int(t), () =>
        switch (CanvasCamera.pane()) {
        | Some((aw, ah)) =>
          CanvasLog.log(
            Printf.sprintf(
              "camera: scored frame -> (%.0f, %.0f)%s",
              f.center.x,
              f.center.y,
              abs_float(f.zoom -. CanvasCamera.zoom_now^) > 0.02
                ? Printf.sprintf(
                    " zoom %.2f -> %.2f",
                    CanvasCamera.zoom_now^,
                    f.zoom,
                  )
                : "",
            ),
          );
          CanvasCamera.animate(
            ~aw,
            ~ah,
            ~zoom=
              abs_float(f.zoom -. CanvasCamera.zoom_now^) > 0.02
                ? Some(f.zoom) : None,
            ~dur=450.,
            ~easing=EaseInOut,
            (f.center.x, f.center.y),
          );
        | None => ()
        }
      ),
    s.frames,
  );
  CanvasCamera.scored_until :=
    CanvasBuffer.now() +. float_of_int(s.total_ms) +. 500.;
  /* the avatar: one timeline through every waypoint, sorted, so the
     keyframe offsets can never run backwards. All in board units: the
     static position becomes the score's END site now (B1: static state =
     timeline end), anchored exactly as the view anchors it. */
  let sorted =
    List.stable_sort(
      ((_, t1), (_, t2)) => compare(t1, t2),
      List.rev(wps^),
    );
  switch (by_id(CanvasView.avatar_dom_id)) {
  | Some(av) when sorted != [] =>
    let end_site =
      List.rev(s.acts)
      |> List.find_map(((_, a): (int, CanvasScore.act)) => site_end(a.at));
    let (ex, ey) =
      switch (end_site) {
      | Some(p) => p
      | None => fst(List.nth(sorted, List.length(sorted) - 1))
      };
    /* where the avatar visibly is NOW (static anchor + in-flight
       translate, minus the box offset), measured here rather than at
       staging so a render nobody staged cannot leave a stale start */
    let start = {
      let px = v =>
        switch (float_of_string_opt(String.trim(v))) {
        | Some(f) => f
        | None =>
          let n = String.length(v);
          n > 2
            ? Option.value(
                ~default=0.,
                float_of_string_opt(String.sub(v, 0, n - 2)),
              )
            : 0.;
        };
      let st0 = Js.Unsafe.get(av, "style");
      let left = px(Js.to_string(Js.Unsafe.get(st0, "left")))
      and top = px(Js.to_string(Js.Unsafe.get(st0, "top")));
      let tf: string =
        Js.to_string(
          Js.Unsafe.get(
            Js.Unsafe.meth_call(
              Js.Unsafe.global##.window,
              "getComputedStyle",
              [|Js.Unsafe.inject(av)|],
            ),
            "transform",
          ),
        );
      let (tx, ty) =
        switch (String.index_opt(tf, '(')) {
        | Some(i) when String.length(tf) > i + 1 =>
          let inner = String.sub(tf, i + 1, String.length(tf) - i - 2);
          switch (
            inner
            |> String.split_on_char(',')
            |> List.filter_map(s => float_of_string_opt(String.trim(s)))
          ) {
          | [_, _, _, _, tx, ty] => (tx, ty)
          | _ => (0., 0.)
          };
        | _ => (0., 0.)
        };
      if (left == 0. && top == 0.) {
        Option.value(
          ~default=fst(List.hd(sorted)),
          CanvasBuffer.avatar_prev^,
        );
      } else {
        (
          left +. tx -. CanvasView.avatar_dx,
          top +. ty -. CanvasView.avatar_dy,
        );
      };
    };
    CanvasBuffer.avatar_site := Some((ex, ey));
    let st = Js.Unsafe.get(av, "style");
    Js.Unsafe.set(
      st,
      "left",
      Js.string(Printf.sprintf("%.1fpx", ex +. CanvasView.avatar_dx)),
    );
    Js.Unsafe.set(
      st,
      "top",
      Js.string(Printf.sprintf("%.1fpx", ey +. CanvasView.avatar_dy)),
    );
    let (_, t_last) = List.nth(sorted, List.length(sorted) - 1);
    let total_ms = max(float_of_int(s.total_ms), t_last) +. settle_ms;
    let frame = ((x, y), at) => [
      (
        "transform",
        str(Printf.sprintf("translate(%.1fpx, %.1fpx)", x -. ex, y -. ey)),
      ),
      ("offset", num(max(0., min(1., at /. total_ms)))),
      ("easing", str("ease-in-out")),
    ];
    /* E1: a timeline that asks the avatar to cover a long way in almost no
       time is a jump; say so in the journal with where and when */
    ignore(
      List.fold_left(
        (((px0, py0), t0), ((px1, py1), t1)) => {
          let d =
            sqrt(
              (px1 -. px0) *. (px1 -. px0) +. (py1 -. py0) *. (py1 -. py0),
            );
          if (d > 40. && d /. max(1., t1 -. t0) > 3.) {
            CanvasLog.log(
              Printf.sprintf(
                "TIMELINE-JUMP: %.0fpx in %.0fms at %.1fs (%.0f,%.0f)->(%.0f,%.0f)",
                d,
                t1 -. t0,
                t1 /. 1000.,
                px0,
                py0,
                px1,
                py1,
              ),
            );
          };
          ((px1, py1), t1);
        },
        (start, 0.),
        sorted,
      ),
    );
    let frames =
      [frame(start, 0.)]
      @ List.map(((p, at)) => frame(p, at), sorted)
      @ [
        [
          ("transform", str("translate(0px, 0px)")),
          ("offset", num(1.)),
          ("easing", str("ease-in-out")),
        ],
      ];
    {
      let anims = Js.Unsafe.meth_call(av, "getAnimations", [||]);
      let n: int = Js.Unsafe.get(anims, "length");
      if (n > 0) {
        CanvasLog.log(
          Printf.sprintf(
            "avatar: %d running animation(s) replaced by the new score",
            n,
          ),
        );
      };
    };
    cancel_anims(av);
    animate(
      av,
      frames,
      [("duration", num(total_ms)), ("easing", str("linear"))],
    );
    CanvasLog.log(
      Printf.sprintf(
        "play: %d act(s), avatar through %d waypoint(s) over %.1fs",
        List.length(s.acts),
        List.length(sorted),
        total_ms /. 1000.,
      ),
    );
  | _ =>
    CanvasLog.log(
      Printf.sprintf(
        "play: %d act(s), no avatar on stage%s",
        List.length(s.acts),
        sorted == [] ? " (no waypoints)" : "",
      ),
    )
  };
};

/* ---- the jump detector (B1): positions before a paced render vs after,
   in board space so camera motion does not count; a move with no
   animation covering it is a JUMP ---- */
let jump_prev: ref(list((string, (float, float)))) = ref([]);
let jump_scroll: ref(option((float, float))) = ref(None);
let jump_snapshot = (~zoom: float): unit => {
  let ids =
    Util.JsUtil.ids_with_prefix("cnode-")
    @ Util.JsUtil.ids_with_prefix("cedge-")
    @ [CanvasView.avatar_dom_id];
  jump_prev :=
    List.filter_map(
      id =>
        by_id(id)
        |> Util.OptUtil.and_then(el => to_board(~zoom, center_of(el)))
        |> Option.map(p => (id, p)),
      ids,
    );
  jump_scroll :=
    (
      switch (CanvasCamera.scroll_el()) {
      | Some(el) =>
        let el = Js.Unsafe.coerce(el);
        Some((el##.scrollLeft, el##.scrollTop));
      | None => None
      }
    );
};
let check_jumps = (~zoom: float): unit => {
  let jumps =
    List.filter_map(
      ((id, (x0, y0))) =>
        switch (by_id(id)) {
        | Some(el) =>
          switch (to_board(~zoom, center_of(el))) {
          | Some((x1, y1)) =>
            let d =
              sqrt((x1 -. x0) *. (x1 -. x0) +. (y1 -. y0) *. (y1 -. y0));
            let anims = Js.Unsafe.meth_call(el, "getAnimations", [||]);
            let n: int = Js.Unsafe.get(anims, "length");
            d > 3. && n == 0
              ? Some(Printf.sprintf("%s %.0fpx", id, d)) : None;
          | None => None
          }
        | None => None
        },
      jump_prev^,
    );
  if (jumps != []) {
    CanvasLog.log("JUMP: " ++ String.concat(", ", jumps));
  };
  switch (jump_scroll^, CanvasCamera.scroll_el()) {
  | (Some((sl0, st0)), Some(el)) =>
    let el = Js.Unsafe.coerce(el);
    let sl: float = el##.scrollLeft
    and st: float = el##.scrollTop;
    if ((abs_float(sl -. sl0) > 2. || abs_float(st -. st0) > 2.)
        && CanvasCamera.inflight^ == None) {
      CanvasLog.log(
        Printf.sprintf("JUMP: camera (%.0f, %.0f) px", sl -. sl0, st -. st0),
      );
    };
  | _ => ()
  };
  jump_prev := [];
  jump_scroll := None;
};

/* run after the current render has been patched into the DOM */
let after_render = (f: unit => unit): unit =>
  later(0., () =>
    ignore(
      Js.Unsafe.meth_call(
        Js.Unsafe.global##.window,
        "requestAnimationFrame",
        [|Js.Unsafe.inject(Js.Unsafe.callback(f))|],
      ),
    )
  );

/* console testers: __canvasStage() stages the next render as an agent
   beat; __canvasEnact("edge") replays the ride on an existing edge;
   __canvasEnactProduct("edge", "productKey", "partKey1|partKey2") replays
   the multi-arg gesture */
let install_testers = (): unit => {
  let g = Js.Unsafe.global;
  if (!Js.Optdef.test(Js.Unsafe.get(g, "__canvasStage"))) {
    Js.Unsafe.set(
      g,
      "__canvasStage",
      Js.Unsafe.callback(() => CanvasBuffer.stage_beat(~lead=true, ())),
    );
    /* __canvasBurst() marks an agent action so the next edit is paced
       and choreographed as an agent's would be */
    Js.Unsafe.set(
      g,
      "__canvasBurst",
      Js.Unsafe.callback(() => {
        CanvasBuffer.note_agent_action();
        CanvasBuffer.fake_busy_until := CanvasBuffer.now() +. 12000.;
      }),
    );
    Js.Unsafe.set(
      g,
      "__constellationStats",
      Js.Unsafe.callback(() =>
        Js.string(
          String.concat("\n", List.rev(CanvasBuffer.turn_summaries^)),
        )
      ),
    );
    Js.Unsafe.set(
      g,
      "__canvasEnact",
      Js.Unsafe.callback((name: Js.t(Js.js_string)) =>
        enact_edges(
          ~zoom=CanvasCamera.zoom_now^,
          [
            {
              ne_name: Js.to_string(name),
              ne_product: None,
            },
          ],
        )
      ),
    );
    Js.Unsafe.set(
      g,
      "__canvasEnactProduct",
      Js.Unsafe.callback(
        (
          name: Js.t(Js.js_string),
          pk: Js.t(Js.js_string),
          parts: Js.t(Js.js_string),
        ) =>
        enact_edges(
          ~zoom=CanvasCamera.zoom_now^,
          [
            {
              ne_name: Js.to_string(name),
              ne_product:
                Some((
                  Js.to_string(pk),
                  String.split_on_char('|', Js.to_string(parts)),
                )),
            },
          ],
        )
      ),
    );
  };
};
