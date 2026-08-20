open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* CanvasView — SVG/HTML rendering of a laid-out CanvasGraph. Edges live in
   an underlying SVG; nodes, labels, values, and the agent avatar are
   absolutely-positioned divs so they can carry stable DOM ids for the
   id-keyed FLIP in Animation.re (positioned via left/top, NOT transform —
   WAAPI transform animations would clobber a transform-based centering). */

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

let pos_style = (~w: float, ~h: float, p: CanvasLayout.pos): Attr.t =>
  Attr.create(
    "style",
    Printf.sprintf(
      "left: %spx; top: %spx; width: %spx; height: %spx;",
      fmt(p.x -. w /. 2.),
      fmt(p.y -. h /. 2.),
      fmt(w),
      fmt(h),
    ),
  );

let kind_cls = (k: CanvasGraph.node_kind): string =>
  switch (k) {
  | Alias => "kind-alias"
  | Builtin => "kind-builtin"
  | Derived => "kind-derived"
  | Ghost => "kind-ghost"
  };

let svg = (name, attrs, children) => Node.create_svg(name, ~attrs, children);

let line = (~cls: list(string), a: CanvasLayout.pos, b: CanvasLayout.pos) =>
  svg(
    "line",
    [
      clss(["canvas-line", ...cls]),
      Attr.create("x1", fmt(a.x)),
      Attr.create("y1", fmt(a.y)),
      Attr.create("x2", fmt(b.x)),
      Attr.create("y2", fmt(b.y)),
    ],
    [],
  );

/* pull the endpoint back so the arrowhead lands on the node's rim */
let shorten =
    (a: CanvasLayout.pos, b: CanvasLayout.pos, by: float): CanvasLayout.pos => {
  let dx = b.x -. a.x
  and dy = b.y -. a.y;
  let len = max(1., sqrt(dx *. dx +. dy *. dy));
  {
    x: b.x -. dx /. len *. by,
    y: b.y -. dy /. len *. by,
  };
};

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

let edge_svg =
    (~radius_of: string => float, el: CanvasLayout.edge_layout): list(Node.t) => {
  let e = el.edge;
  let cls =
    [e.main ? "edge-main" : "edge-helper"]
    @ (e.e_hole ? ["edge-hole"] : [])
    @ (e.e_err ? ["edge-err"] : []);
  if (el.endo) {
    let r = radius_of(e.dst) +. 16. +. float_of_int(el.orbit_rank) *. 15.;
    let orbit =
      svg(
        "circle",
        [
          clss(["canvas-orbit", ...cls]),
          Attr.create("cx", fmt(el.dst_p.x)),
          Attr.create("cy", fmt(el.dst_p.y)),
          Attr.create("r", fmt(r)),
        ],
        [],
      );
    let inputs =
      List.map(
        p => line(~cls=["edge-input", ...cls], p, el.jct),
        el.src_ps,
      );
    [orbit, ...inputs];
  } else {
    let tip = shorten(el.jct, el.dst_p, radius_of(el.edge.dst) +. 4.);
    let main_line =
      svg(
        "line",
        [
          clss(["canvas-line", "edge-arrow", ...cls]),
          Attr.create("x1", fmt(el.jct.x)),
          Attr.create("y1", fmt(el.jct.y)),
          Attr.create("x2", fmt(tip.x)),
          Attr.create("y2", fmt(tip.y)),
          Attr.create("marker-end", "url(#cnv-arrow)"),
        ],
        [],
      );
    let inputs =
      List.length(el.src_ps) == 1
        ? []  /* single arg: jct sits on the src→dst line, skip stub */
        : List.map(
            p => line(~cls=["edge-input", ...cls], p, el.jct),
            el.src_ps,
          );
    let single =
      switch (el.src_ps) {
      | [p] => [line(~cls=["edge-input", ...cls], p, el.jct)]
      | _ => []
      };
    inputs @ single @ [main_line];
  };
};

let edge_label = (~inject_jump, el: CanvasLayout.edge_layout): Node.t => {
  let e = el.edge;
  let tooltip =
    e.e_name
    ++ " : "
    ++ e.e_ty
    ++ (
      switch (e.e_doc) {
      | Some(d) => "\n" ++ d
      | None => ""
      }
    );
  div(
    ~attrs=[
      Attr.id(edge_dom_id(e.e_name)),
      clss(
        ["canvas-edge-label"]
        @ (e.main ? ["edge-main"] : [])
        @ (e.e_hole ? ["edge-hole"] : [])
        @ (e.e_err ? ["edge-err"] : []),
      ),
      Attr.create(
        "style",
        Printf.sprintf(
          "left: %spx; top: %spx;",
          fmt(el.label_p.x),
          fmt(el.label_p.y),
        ),
      ),
      Attr.title(tooltip),
      Attr.on_click(_ => inject_jump(e.e_id)),
    ],
    [text(e.e_name), ...List.map(test_pip, e.tests)],
  );
};

let node_view = (~inject_jump, nl: CanvasLayout.node_layout): Node.t => {
  let n = nl.node;
  let d = nl.r *. 2.;
  let tooltip =
    n.label
    ++ (n.ctrs == [] ? "" : " = " ++ String.concat(" + ", n.ctrs))
    ++ (
      switch (n.n_doc) {
      | Some(doc) => "\n" ++ doc
      | None => ""
      }
    );
  let click_attrs =
    switch (n.n_id) {
    | Some(id) => [Attr.on_click(_ => inject_jump(id)), clss(["clickable"])]
    | None => []
    };
  div(
    ~attrs=
      [
        Attr.id(node_dom_id(n.key)),
        clss(
          ["canvas-node", kind_cls(n.kind)] @ (n.n_err ? ["node-err"] : []),
        ),
        pos_style(~w=d, ~h=d, nl.p),
        Attr.title(tooltip),
      ]
      @ click_attrs,
    [span(~attrs=[clss(["canvas-node-label"])], [text(n.label)])],
  );
};

let value_view = (~inject_jump, vl: CanvasLayout.value_layout): Node.t => {
  let v = vl.value;
  div(
    ~attrs=[
      Attr.id("cval-" ++ sanitize(v.v_name)),
      clss(["canvas-value"] @ (v.v_err ? ["node-err"] : [])),
      Attr.create(
        "style",
        Printf.sprintf("left: %spx; top: %spx;", fmt(vl.p.x), fmt(vl.p.y)),
      ),
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
      Attr.create(
        "style",
        Printf.sprintf(
          "left: %spx; top: %spx;",
          fmt(p.x +. 14.),
          fmt(p.y -. 34.),
        ),
      ),
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
  let defs =
    svg(
      "defs",
      [],
      [
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
      [defs, ...List.concat_map(edge_svg(~radius_of), lay.edges)],
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
    @ List.map(node_view(~inject_jump), lay.nodes)
    @ List.map(edge_label(~inject_jump), lay.edges)
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
