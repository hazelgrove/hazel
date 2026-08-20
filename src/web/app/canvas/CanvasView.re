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

let test_pip = (t: CanvasGraph.test_info): Node.t => {
  let cls =
    switch (t.status) {
    | Some(Pass) => "pip-pass"
    | Some(Fail) => "pip-fail"
    | Some(Indet) => "pip-indet"
    | None => "pip-unknown"
    };
  div(~attrs=[clss(["test-pip", cls])], []);
};

let edge_classes =
    (~focused: option(string)=None, e: CanvasGraph.edge): list(string) =>
  [e.main ? "edge-main" : "edge-helper"]
  @ (e.e_hole ? ["edge-hole"] : [])
  @ (e.e_err ? ["edge-err"] : [])
  @ (focused == Some(e.e_name) ? ["edge-focused"] : []);

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
              fmt(el.dst_p.x),
              fmt(el.dst_p.y),
            ),
          ),
          Attr.create("marker-end", "url(#cnv-arrow)"),
        ],
        [],
      ),
    ];
  };
};

let formation_svg = ((cp, pp): (CanvasLayout.pos, CanvasLayout.pos)): Node.t => {
  /* gentle curve from component toward its product */
  let mx = (cp.x +. pp.x) /. 2.;
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
          fmt(pp.x),
          fmt(pp.y),
        ),
      ),
      Attr.create("marker-end", "url(#cnv-arrow-sm)"),
    ],
    [],
  );
};

let dep_link_svg = ((dp, np): (CanvasLayout.pos, CanvasLayout.pos)): Node.t =>
  svg(
    "line",
    [
      clss(["canvas-dep"]),
      Attr.create("x1", fmt(dp.x)),
      Attr.create("y1", fmt(dp.y)),
      Attr.create("x2", fmt(np.x)),
      Attr.create("y2", fmt(np.y)),
      Attr.create("marker-end", "url(#cnv-arrow-dep)"),
    ],
    [],
  );

let edge_label =
    (
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
    [text(e.e_name), ...List.map(test_pip, e.tests)],
  );
};

let node_view =
    (
      ~on_node_click: CanvasGraph.tynode => Effect.t(unit),
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
  let click_attrs =
    n.kind == Product
      ? [] : [Attr.on_click(_ => on_node_click(n)), clss(["clickable"])];
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
      ~on_node_click: CanvasGraph.tynode => Effect.t(unit),
      ~focused: option(string),
      ~avatar: option((CanvasLayout.pos, string)),
      ~loose_tests: list(CanvasGraph.test_info),
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
        Attr.create("refX", fmt(size *. 0.8)),
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
            Attr.create("refX", "7"),
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
      @ List.concat_map(edge_svg(~focused, ~radius_of), lay.edges),
    );
  let loose =
    loose_tests == []
      ? []
      : [
        div(
          ~attrs=[clss(["canvas-loose-tests"])],
          [text("tests: "), ...List.map(test_pip, loose_tests)],
        ),
      ];
  div(
    ~attrs=[
      clss(["canvas-root"]),
      Attr.create(
        "style",
        Printf.sprintf(
          "width: %spx; height: %spx;",
          fmt(lay.width),
          fmt(lay.height),
        ),
      ),
    ],
    [edges_svg]
    @ List.map(node_view(~on_node_click), lay.nodes)
    @ List.map(edge_label(~focused, ~on_edge_click), lay.edges)
    @ List.map(value_view(~inject_jump), lay.values)
    @ (
      switch (avatar) {
      | Some(a) => [avatar_view(a)]
      | None => []
      }
    )
    @ loose,
  );
};
