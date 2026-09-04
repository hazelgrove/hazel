/* CanvasEnact — choreography derived from the graph DIFF of an agent
   beat, so it is a projection of the structural edit whatever tool made
   it. A new function edge is enacted as the ƒ gesture: the avatar
   travels from the edge's domain to its codomain PULLING the arrow along
   (the stroke reveals under it), then steps back to the pill, which
   fades in. Runs on the rendered DOM one frame after the beat lands
   (Web Animations), and every animation ends at the identity, so nothing
   here fights the vdom. */

open Js_of_ocaml;

let travel_ms = 700.; /* domain -> codomain */
let settle_ms = 220.; /* codomain -> the pill */
let lead_ms = float_of_int(CanvasBuffer.lead_ms);
let samples = 18;
/* more new edges than this in one beat = a rewrite, not a gesture */
let max_edges = 3;

let by_id = (id: string): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(
    Js.Unsafe.global##.document##getElementById(Js.string(id)),
  );

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

/* the path's length and evenly spaced points along it, in SCREEN px */
let sample_path =
    (path: Js.t(Dom_html.element)): (float, list((float, float))) => {
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
            [|
              Js.Unsafe.inject(
                total *. float_of_int(i) /. float_of_int(samples),
              ),
            |],
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
      el: Js.t(Dom_html.element),
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
let str = (s: string): Js.Unsafe.any => Js.Unsafe.inject(Js.string(s));
let num = (f: float): Js.Unsafe.any => Js.Unsafe.inject(f);

/* one edge: reveal the stroke while the avatar rides it */
let enact_edge =
    (~zoom: float, ~avatar: option(Js.t(Dom_html.element)), name: string)
    : bool =>
  switch (by_id(CanvasView.path_dom_id(name))) {
  | None => false
  | Some(path) =>
    let (total, pts) = sample_path(path);
    if (total < 8.) {
      false;
    } else {
      /* the arrowhead would give the destination away: hide it until
         the avatar arrives */
      let marker = Js.Unsafe.get(path, "style");
      let marker_attr =
        Js.Opt.to_option(
          Js.Unsafe.meth_call(path, "getAttribute", [|str("marker-end")|]),
        );
      ignore(
        Js.Unsafe.meth_call(
          path,
          "setAttribute",
          [|str("marker-end"), str("none")|],
        ),
      );
      ignore(marker);
      ignore(
        Js.Unsafe.meth_call(
          path,
          "setAttribute",
          [|
            str("stroke-dasharray"),
            str(Printf.sprintf("%.1f %.1f", total, total)),
          |],
        ),
      );
      animate(
        path,
        [
          [("strokeDashoffset", num(total))],
          [("strokeDashoffset", num(0.))],
        ],
        [
          ("duration", num(travel_ms)),
          ("delay", num(lead_ms)),
          ("easing", str("cubic-bezier(0.65, 0, 0.35, 1)")),
          ("fill", str("backwards")),
        ],
      );
      ignore(
        Js.Unsafe.global##setTimeout(
          Js.Unsafe.callback(() =>
            switch (marker_attr) {
            | Some(m) =>
              ignore(
                Js.Unsafe.meth_call(
                  path,
                  "setAttribute",
                  [|str("marker-end"), Js.Unsafe.inject(m)|],
                ),
              )
            | None => ()
            }
          ),
          lead_ms +. travel_ms,
        ),
      );
      /* the pill appears once the arrow is drawn */
      switch (by_id(CanvasView.edge_dom_id(name))) {
      | Some(pill) =>
        animate(
          pill,
          [[("opacity", num(0.))], [("opacity", num(1.))]],
          [
            ("duration", num(settle_ms)),
            ("delay", num(lead_ms +. travel_ms)),
            ("fill", str("backwards")),
          ],
        )
      | None => ()
      };
      /* the avatar rides the path: hold at the domain during the lead,
         travel to the codomain, then step back to where the beat put it */
      switch (avatar) {
      | Some(av) =>
        let rect = Js.Unsafe.meth_call(av, "getBoundingClientRect", [||]);
        let ax: float =
          Js.Unsafe.get(rect, "left") +. Js.Unsafe.get(rect, "width") /. 2.
        and ay: float =
          Js.Unsafe.get(rect, "top") +. Js.Unsafe.get(rect, "height") /. 2.;
        let total_ms = lead_ms +. travel_ms +. settle_ms;
        let n = float_of_int(List.length(pts) - 1);
        let frame = ((x, y), off) => [
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
          ("offset", num(off)),
        ];
        let travel =
          List.mapi(
            (i, p) =>
              frame(
                p,
                (lead_ms +. travel_ms *. float_of_int(i) /. n) /. total_ms,
              ),
            pts,
          );
        let frames =
          [frame(List.hd(pts), 0.)]
          @ travel
          @ [
            [
              ("transform", str("translate(0px, 0px)")),
              ("offset", num(1.)),
            ],
          ];
        /* the beat's own FLIP hop would fight the ride */
        let anims = Js.Unsafe.meth_call(av, "getAnimations", [||]);
        let len: int = Js.Unsafe.get(anims, "length");
        for (i in 0 to len - 1) {
          ignore(
            Js.Unsafe.meth_call(Js.Unsafe.get(anims, i), "cancel", [||]),
          );
        };
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
let enact_edges = (~zoom: float, names: list(string)): unit =>
  if (names != [] && List.length(names) <= max_edges) {
    let avatar = by_id(CanvasView.avatar_dom_id);
    let done_ = List.filter(name => enact_edge(~zoom, ~avatar, name), names);
    if (done_ != []) {
      CanvasLog.log(
        Printf.sprintf(
          "enact: %d edge(s) drawn by the avatar (%s)",
          List.length(done_),
          String.concat(", ", done_),
        ),
      );
    };
  };

/* run after the current render has been patched into the DOM */
let after_render = (f: unit => unit): unit =>
  ignore(
    Js.Unsafe.global##setTimeout(
      Js.Unsafe.callback(() =>
        ignore(
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.window,
            "requestAnimationFrame",
            [|Js.Unsafe.inject(Js.Unsafe.callback(f))|],
          ),
        )
      ),
      0,
    ),
  );

/* console tester: __canvasEnact("edge name") replays the gesture on an
   existing edge (pass the pill's label as shown, e.g. "quiz") */
let install_testers = (): unit => {
  let g = Js.Unsafe.global;
  if (!Js.Optdef.test(Js.Unsafe.get(g, "__canvasEnact"))) {
    Js.Unsafe.set(
      g,
      "__canvasEnact",
      Js.Unsafe.callback((name: Js.t(Js.js_string)) =>
        enact_edges(~zoom=CanvasCamera.zoom_now^, [Js.to_string(name)])
      ),
    );
  };
};
