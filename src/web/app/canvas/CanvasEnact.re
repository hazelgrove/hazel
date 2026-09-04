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

/* draw a stroke on from its start over dur ms starting at delay */
let reveal = (~delay: float, ~dur: float, el): unit => {
  cancel_anims(el);
  let total: float = Js.Unsafe.meth_call(el, "getTotalLength", [||]);
  set_attr(
    el,
    "stroke-dasharray",
    Printf.sprintf("%.1f %.1f", total, total),
  );
  animate(
    el,
    [
      [("strokeDashoffset", num(total))],
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
      set_attr(
        path,
        "stroke-dasharray",
        Printf.sprintf("%.1f %.1f", total, total),
      );
      animate(
        path,
        [
          [("strokeDashoffset", num(total))],
          [("strokeDashoffset", num(0.))],
        ],
        [
          ("duration", num(travel_ms)),
          ("delay", num(t_edge)),
          ("easing", str("cubic-bezier(0.65, 0, 0.35, 1)")),
          ("fill", str("backwards")),
        ],
      );
      switch (marker) {
      | Some(m) =>
        later(t_edge +. travel_ms, () => set_attr(path, "marker-end", m))
      | None => ()
      };
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
    cancel_anims(av);
    animate(
      av,
      frames,
      [("duration", num(total_ms)), ("easing", str("ease-in-out"))],
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
