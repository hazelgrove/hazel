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

/* one edge: (visit the domain types, form the product dot,) reveal the
   stroke while the avatar rides it, then let the pill in */
let enact_edge =
    (~zoom: float, ~avatar: option(Js.t(Dom_html.element)), ne: new_edge)
    : bool =>
  switch (by_id(CanvasView.path_dom_id(ne.ne_name))) {
  | None => false
  | Some(path) =>
    /* the beat pass may already be revealing this path generically;
       the ride replaces it */
    cancel_anims(path);
    let (total, pts) = sample_path(path);
    if (total < 8.) {
      false;
    } else {
      /* ---- multi-arg prelude: visits + formation ---- */
      /* waypoints the avatar passes before the ride: (screen point, at ms) */
      let prelude: ref(list(((float, float), float))) = ref([]);
      let t_edge =
        switch (ne.ne_product) {
        | Some((pk, parts)) =>
          switch (by_id(CanvasView.node_dom_id(pk))) {
          | None => lead_ms
          | Some(dot) =>
            let part_els =
              List.filter_map(k => by_id(CanvasView.node_dom_id(k)), parts);
            let n = float_of_int(List.length(part_els));
            let t_visited = lead_ms +. visit_ms *. n;
            prelude :=
              List.mapi(
                (i, el) =>
                  (
                    center_of(el),
                    lead_ms +. visit_ms *. float_of_int(i + 1),
                  ),
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
        | None => lead_ms
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
      /* ---- the avatar: hold, (visit, visit, dot,) ride, step back ---- */
      switch (avatar) {
      | Some(av) =>
        let (ax, ay) = center_of(av);
        let total_ms = t_edge +. travel_ms +. settle_ms;
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
          ("offset", num(min(1., at /. total_ms))),
        ];
        let n = float_of_int(List.length(pts) - 1);
        let ride =
          List.mapi(
            (i, p) => frame(p, t_edge +. travel_ms *. float_of_int(i) /. n),
            pts,
          );
        let hold_at = List.hd(pts);
        let frames =
          [frame(prelude^ == [] ? hold_at : fst(List.hd(prelude^)), 0.)]
          @ List.map(((p, at)) => frame(p, at), prelude^)
          @ ride
          @ [
            [
              ("transform", str("translate(0px, 0px)")),
              ("offset", num(1.)),
            ],
          ];
        cancel_anims(av);
        animate(
          av,
          frames,
          [("duration", num(total_ms)), ("easing", str("ease-in-out"))],
        );
      | None => ()
      };
      true;
    };
  };

/* enact the new edges of a beat (called one frame after it renders) */
let enact_edges = (~zoom: float, edges: list(new_edge)): unit =>
  if (edges != [] && List.length(edges) <= max_edges) {
    let avatar = by_id(CanvasView.avatar_dom_id);
    let done_ = List.filter(ne => enact_edge(~zoom, ~avatar, ne), edges);
    if (done_ != []) {
      CanvasLog.log(
        Printf.sprintf(
          "enact: %d edge(s) drawn by the avatar (%s)",
          List.length(done_),
          String.concat(
            ", ",
            List.map(
              ne =>
                ne.ne_name
                ++ (
                  switch (ne.ne_product) {
                  | Some((_, parts)) =>
                    Printf.sprintf(" via %d-ary product", List.length(parts))
                  | None => ""
                  }
                ),
              done_,
            ),
          ),
        ),
      );
    };
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
