open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* CanvasView — SVG/HTML rendering of a laid-out CanvasGraph. Edges live in
   an underlying SVG (horizontal-tangent béziers; orbits for single-arg
   endofunctions); nodes, labels, values, and the agent avatar are
   absolutely-positioned divs so they can carry stable DOM ids for the
   id-keyed FLIP in Animation.re. Elements are anchored via left/top and
   centered with the standalone CSS `translate` property — never the
   `transform` property, which WAAPI FLIP animations would override
   mid-flight (the label-jump bug). */

let fmt = f => Printf.sprintf("%.1f", f);

let sanitize = (s: string): string =>
  String.map(
    c =>
      switch (c) {
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' => c
      | _ => '-'
      },
    s,
  );

let node_dom_id = (key: string): string => "cnode-" ++ sanitize(key);
let edge_dom_id = (name: string): string => "cedge-" ++ sanitize(name);
let avatar_dom_id = "canvas-avatar";

let anchor_style = (p: CanvasLayout.pos): Attr.t =>
  Attr.create(
    "style",
    Printf.sprintf("left: %spx; top: %spx;", fmt(p.x), fmt(p.y)),
  );

let kind_cls = (k: CanvasGraph.node_kind): string =>
  switch (k) {
  | Alias => "kind-alias"
  | Builtin => "kind-builtin"
  | Derived => "kind-derived"
  | Ghost => "kind-ghost"
  | Product => "kind-product"
  };

let svg = (name, attrs, children) => Node.create_svg(name, ~attrs, children);

let test_pip = (~on_click=?, t: CanvasGraph.test_info): Node.t => {
  let cls =
    switch (t.status) {
    | Some(Pass) => "pip-pass"
    | Some(Fail) => "pip-fail"
    | Some(Indet) => "pip-indet"
    | None => "pip-unknown"
    };
  let click =
    switch (on_click) {
    | Some(f) => [Attr.on_click(_ => f(t)), Attr.title("select this test")]
    | None => []
    };
  div(~attrs=[clss(["test-pip", cls]), ...click], []);
};

let edge_classes =
    (~focused: option(string)=None, e: CanvasGraph.edge): list(string) =>
  [e.main ? "edge-main" : "edge-helper"]
  @ (e.e_hole ? ["edge-hole"] : [])
  @ (e.e_err ? ["edge-err"] : [])
  @ (focused == Some(e.e_name) ? ["edge-focused"] : []);

/* End curves at the arrowhead BASE: pull the path end back along its
   final tangent so the marker (refX=0) occupies the gap and the tip
   lands where the rim point was. */
let pull_back =
    (p: CanvasLayout.pos, toward: CanvasLayout.pos, d: float)
    : CanvasLayout.pos => {
  let dx = toward.x -. p.x
  and dy = toward.y -. p.y;
  let len = max(1., Float.hypot(dx, dy));
  {
    x: p.x +. dx /. len *. d,
    y: p.y +. dy /. len *. d,
  };
};

let edge_svg =
    (
      ~focused: option(string),
      ~radius_of: string => float,
      el: CanvasLayout.edge_layout,
    )
    : list(Node.t) => {
  let e = el.edge;
  let cls = edge_classes(~focused, e);
  if (el.endo) {
    let r = radius_of(e.dst) +. 16. +. float_of_int(el.orbit_rank) *. 15.;
    [
      svg(
        "circle",
        [
          clss(["canvas-orbit", ...cls]),
          Attr.create("cx", fmt(el.dst_p.x)),
          Attr.create("cy", fmt(el.dst_p.y)),
          Attr.create("r", fmt(r)),
        ],
        [],
      ),
    ];
  } else {
    let dst = pull_back(el.dst_p, el.c2, 7.);
    let marker =
      List.mem("edge-focused", cls)
        ? "url(#cnv-arrow-focus)" : "url(#cnv-arrow)";
    [
      svg(
        "path",
        [
          clss(["canvas-line", "edge-arrow", ...cls]),
          Attr.create(
            "d",
            Printf.sprintf(
              "M %s,%s C %s,%s %s,%s %s,%s",
              fmt(el.src_p.x),
              fmt(el.src_p.y),
              fmt(el.c1.x),
              fmt(el.c1.y),
              fmt(el.c2.x),
              fmt(el.c2.y),
              fmt(dst.x),
              fmt(dst.y),
            ),
          ),
          Attr.create("marker-end", marker),
        ],
        [],
      ),
    ];
  };
};

let formation_svg = ((cp, pp): (CanvasLayout.pos, CanvasLayout.pos)): Node.t => {
  /* gentle curve from component toward its product */
  let mx = (cp.x +. pp.x) /. 2.;
  let pp' =
    pull_back(
      pp,
      {
        x: mx,
        y: pp.y,
      },
      5.,
    );
  svg(
    "path",
    [
      clss(["canvas-formation"]),
      Attr.create(
        "d",
        Printf.sprintf(
          "M %s,%s C %s,%s %s,%s %s,%s",
          fmt(cp.x),
          fmt(cp.y),
          fmt(mx),
          fmt(cp.y),
          fmt(mx),
          fmt(pp.y),
          fmt(pp'.x),
          fmt(pp'.y),
        ),
      ),
      Attr.create("marker-end", "url(#cnv-arrow-sm)"),
    ],
    [],
  );
};

/* faint dotted connector from a displaced label chip back to its curve,
   so separation never orphans a name from its edge */
let leader_svg = (el: CanvasLayout.edge_layout): list(Node.t) => {
  let a = el.label_anchor
  and p = el.label_p;
  Float.hypot(a.x -. p.x, a.y -. p.y) < 30.
    ? []
    : [
      svg(
        "line",
        [
          clss(["canvas-leader"]),
          Attr.create("x1", fmt(p.x)),
          Attr.create("y1", fmt(p.y -. 8.)),
          Attr.create("x2", fmt(a.x)),
          Attr.create("y2", fmt(a.y)),
        ],
        [],
      ),
    ];
};

let dep_link_svg = ((dp, np): (CanvasLayout.pos, CanvasLayout.pos)): Node.t => {
  let np' = pull_back(np, dp, 5.);
  svg(
    "line",
    [
      clss(["canvas-dep"]),
      Attr.create("x1", fmt(dp.x)),
      Attr.create("y1", fmt(dp.y)),
      Attr.create("x2", fmt(np'.x)),
      Attr.create("y2", fmt(np'.y)),
      Attr.create("marker-end", "url(#cnv-arrow-dep)"),
    ],
    [],
  );
};

let edge_label =
    (
      ~inject_jump: Haz3lcore.Id.t => Effect.t(unit),
      ~focused: option(string),
      ~on_edge_click: CanvasGraph.edge => Effect.t(unit),
      el: CanvasLayout.edge_layout,
    )
    : Node.t => {
  let e = el.edge;
  let test_note = {
    let n = List.length(e.tests);
    let passing =
      List.length(
        List.filter(
          (t: CanvasGraph.test_info) => t.status == Some(Pass),
          e.tests,
        ),
      );
    n == 0 ? "" : Printf.sprintf("\n%d test(s), %d passing", n, passing);
  };
  let tooltip =
    e.e_name
    ++ " : "
    ++ e.e_ty
    ++ (
      switch (e.e_doc) {
      | Some(d) => "\n" ++ d
      | None => ""
      }
    )
    ++ test_note;
  div(
    ~attrs=[
      Attr.id(edge_dom_id(e.e_name)),
      clss(["canvas-edge-label", ...edge_classes(~focused, e)]),
      anchor_style(el.label_p),
      Attr.title(tooltip),
      Attr.on_click(_ => on_edge_click(e)),
    ],
    [
      text(e.e_name),
      ...List.map(
           test_pip(~on_click=(t: CanvasGraph.test_info) =>
             inject_jump(t.t_id)
           ),
           e.tests,
         ),
    ],
  );
};

let node_view =
    (
      ~on_node_mousedown:
         (
           CanvasGraph.tynode,
           Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.mouseEvent)
         ) =>
         Effect.t(unit),
      nl: CanvasLayout.node_layout,
    )
    : Node.t => {
  let n = nl.node;
  let d = nl.r *. 2.;
  let tooltip =
    (n.kind == Product ? n.key : n.label)
    ++ (n.ctrs == [] ? "" : " = " ++ String.concat(" + ", n.ctrs))
    ++ (
      switch (n.n_doc) {
      | Some(doc) => "\n" ++ doc
      | None => ""
      }
    );
  let click_attrs = [
    Attr.on_mousedown(evt => on_node_mousedown(n, evt)),
    clss(["clickable"]),
  ];
  let label_nodes =
    n.label == ""
      ? []
      : [span(~attrs=[clss(["canvas-node-label"])], [text(n.label)])];
  div(
    ~attrs=
      [
        Attr.id(node_dom_id(n.key)),
        clss(
          ["canvas-node", kind_cls(n.kind)] @ (n.n_err ? ["node-err"] : []),
        ),
        Attr.create(
          "style",
          Printf.sprintf(
            "left: %spx; top: %spx; width: %spx; height: %spx;",
            fmt(nl.p.x),
            fmt(nl.p.y),
            fmt(d),
            fmt(d),
          ),
        ),
        Attr.title(tooltip),
      ]
      @ click_attrs,
    label_nodes,
  );
};

let value_view = (~inject_jump, vl: CanvasLayout.value_layout): Node.t => {
  let v = vl.value;
  div(
    ~attrs=[
      Attr.id("cval-" ++ sanitize(v.v_name)),
      clss(["canvas-value"] @ (v.v_err ? ["node-err"] : [])),
      anchor_style(vl.p),
      Attr.title(v.v_name ++ " : " ++ v.v_ty),
      Attr.on_click(_ => inject_jump(v.v_id)),
    ],
    [text("● " ++ v.v_name)],
  );
};

let avatar_view = ((p, state): (CanvasLayout.pos, string)): Node.t =>
  div(
    ~attrs=[
      Attr.id(avatar_dom_id),
      clss(["canvas-avatar"] @ (state == "" ? [] : ["avatar-" ++ state])),
      anchor_style({
        x: p.x +. 14.,
        y: p.y -. 34.,
      }),
      Attr.title("the agent is here"),
    ],
    [
      text(
        "@"
        ++ (
          switch (state) {
          | "edit" => {js|✎|js}
          | "wait" => {js|⌛|js}
          | "err" => "!"
          | _ => ""
          }
        ),
      ),
    ],
  );

let view =
    (
      ~inject_jump: Haz3lcore.Id.t => Effect.t(unit),
      ~on_edge_click: CanvasGraph.edge => Effect.t(unit),
      ~on_node_mousedown:
         (
           CanvasGraph.tynode,
           Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.mouseEvent)
         ) =>
         Effect.t(unit),
      ~on_canvas_click: option(((float, float)) => Effect.t(unit))=None,
      ~zoom: float=1.,
      /* pane size in layout px: the root grows to fill it so the dot
         field covers the whole visible canvas */
      ~min_size: (float, float)=(0., 0.),
      ~focused: option(string),
      ~avatar: option((CanvasLayout.pos, string)),
      /* streaming reasoning tail shown in a small bubble by the avatar */
      ~avatar_bubble: option(string)=None,
      ~loose_tests as _: list(CanvasGraph.test_info),
      lay: CanvasLayout.t,
    )
    : Node.t => {
  let radius_of = (k: string): float =>
    List.find_opt(
      (nl: CanvasLayout.node_layout) => nl.node.key == k,
      lay.nodes,
    )
    |> Option.map((nl: CanvasLayout.node_layout) => nl.r)
    |> Option.value(~default=CanvasLayout.base_radius);
  let mk_marker = (~id, ~size, ~cls) =>
    svg(
      "marker",
      [
        Attr.id(id),
        Attr.create("markerWidth", fmt(size)),
        Attr.create("markerHeight", fmt(size)),
        Attr.create("refX", "0"),
        Attr.create("refY", fmt(size /. 2.)),
        Attr.create("orient", "auto"),
        Attr.create("markerUnits", "userSpaceOnUse"),
      ],
      [
        svg(
          "path",
          [
            Attr.create(
              "d",
              Printf.sprintf(
                "M0,0 L%s,%s L0,%s z",
                fmt(size),
                fmt(size /. 2.),
                fmt(size),
              ),
            ),
            clss([cls]),
          ],
          [],
        ),
      ],
    );
  let small_markers = [
    mk_marker(~id="cnv-arrow-sm", ~size=6., ~cls="canvas-arrowhead-sm"),
    mk_marker(~id="cnv-arrow-dep", ~size=6., ~cls="canvas-arrowhead-dep"),
    mk_marker(~id="cnv-arrow-focus", ~size=9., ~cls="canvas-arrowhead-focus"),
  ];
  let defs =
    svg(
      "defs",
      [],
      small_markers
      @ [
        svg(
          "marker",
          [
            Attr.id("cnv-arrow"),
            Attr.create("markerWidth", "9"),
            Attr.create("markerHeight", "9"),
            Attr.create("refX", "0"),
            Attr.create("refY", "4.5"),
            Attr.create("orient", "auto"),
            Attr.create("markerUnits", "userSpaceOnUse"),
          ],
          [
            svg(
              "path",
              [
                Attr.create("d", "M0,0 L9,4.5 L0,9 z"),
                clss(["canvas-arrowhead"]),
              ],
              [],
            ),
          ],
        ),
      ],
    );
  let edges_svg =
    svg(
      "svg",
      [
        clss(["canvas-edges"]),
        Attr.create("width", fmt(lay.width)),
        Attr.create("height", fmt(lay.height)),
      ],
      [defs]
      @ List.map(dep_link_svg, lay.dep_links)
      @ List.map(formation_svg, lay.formations)
      @ List.concat_map(leader_svg, lay.edges)
      @ List.concat_map(edge_svg(~focused, ~radius_of), lay.edges),
    );
  let bg_attrs =
    switch (on_canvas_click) {
    | Some(f) => [
        Attr.on_mousedown(evt => {
          open Js_of_ocaml;
          /* only true background presses (nodes/labels are their own
             targets; the edges svg is pointer-events: none) */
          let tgt = Js.Unsafe.coerce(evt)##.target;
          let cur = Js.Unsafe.coerce(evt)##.currentTarget;
          let same: bool =
            Js.to_bool(
              Js.Unsafe.coerce(
                Js.Unsafe.meth_call(
                  cur,
                  "isSameNode",
                  [|Js.Unsafe.inject(tgt)|],
                ),
              ),
            );
          if (same) {
            let rect =
              Js.Unsafe.meth_call(cur, "getBoundingClientRect", [||]);
            let left: float = Js.Unsafe.coerce(rect)##.left;
            let top: float = Js.Unsafe.coerce(rect)##.top;
            let x: int = Js.Unsafe.coerce(evt)##.clientX;
            let y: int = Js.Unsafe.coerce(evt)##.clientY;
            /* CSS zoom scales client rects; map back to layout px */
            f((
              (float_of_int(x) -. left) /. zoom,
              (float_of_int(y) -. top) /. zoom,
            ));
          } else {
            Effect.Ignore;
          };
        }),
      ]
    | None => []
    };
  div(
    ~attrs=
      [
        clss(["canvas-root"] @ (on_canvas_click == None ? [] : ["placing"])),
        Attr.create(
          "style",
          {
            /* dot pitch loops across zoom levels: as cells visually
               outgrow ~1.4x the base pitch the grid subdivides, and
               below ~0.7x it coarsens — apparent density stays put */
            let lvl = 2. ** Float.round(Float.log2(zoom));
            let pitch = 14. /. lvl;
            let (mw, mh) = min_size;
            Printf.sprintf(
              "width: %spx; height: %spx; zoom: %s; --dot-pitch: %spx; --dot-r: %.3fpx; --dot-fade: %.3fpx;",
              fmt(max(lay.width, mw)),
              fmt(max(lay.height, mh)),
              /* zoom needs full precision: %.1f rounds a pane-fitting
                 zoom up and the min_size-floored root overflows by the
                 excess, re-summoning the scrollbars fit just removed */
              Printf.sprintf("%.4f", zoom),
              fmt(pitch),
              /* dots keep a constant VISUAL size across zoom levels */
              0.75 /. zoom,
              1.1 /. zoom,
            );
          },
        ),
      ]
      @ bg_attrs,
    [edges_svg]
    @ List.map(node_view(~on_node_mousedown), lay.nodes)
    @ List.map(
        edge_label(~inject_jump, ~focused, ~on_edge_click),
        lay.edges,
      )
    @ List.map(value_view(~inject_jump), lay.values)
    @ (
      switch (avatar) {
      | Some((p, _) as a) =>
        [avatar_view(a)]
        @ (
          switch (avatar_bubble) {
          | Some(txt) => [
              div(
                ~attrs=[
                  clss(["canvas-avatar-bubble"]),
                  Attr.create(
                    "style",
                    Printf.sprintf(
                      "left: %spx; top: %spx;",
                      fmt(p.x +. 24.),
                      fmt(p.y -. 40.),
                    ),
                  ),
                ],
                [text(txt)],
              ),
            ]
          | None => []
          }
        )
      | None => []
      }
    ),
  );
};
