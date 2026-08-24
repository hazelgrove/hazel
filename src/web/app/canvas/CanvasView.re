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
let value_dom_id = (name: string): string => "cval-" ++ sanitize(name);
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
          Attr.id("corbit-" ++ sanitize(e.e_name)),
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
          Attr.id("cpath-" ++ sanitize(e.e_name)),
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

let formation_svg =
    (
      i: int,
      (_, _, cp, pp): (string, string, CanvasLayout.pos, CanvasLayout.pos),
    )
    : Node.t => {
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
      Attr.id("cform-" ++ string_of_int(i)),
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
          Attr.id("clead-" ++ sanitize(el.edge.e_name)),
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

let dep_link_svg =
    (
      i: int,
      (_, _, dp, np): (string, string, CanvasLayout.pos, CanvasLayout.pos),
    )
    : Node.t => {
  let np' = pull_back(np, dp, 5.);
  svg(
    "line",
    [
      Attr.id("cdep-" ++ string_of_int(i)),
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

/* ---- module hulls: shared geometry ----
   One source of truth for the metaball circle sets, used by the render,
   the drag-time layout applier (CanvasSidebar), and the jelly springs.
   Ids are unique per (viewing path, element): exact-owner copies get
   bare ids, ancestor copies a --v<path> suffix — every circle is
   addressable. */
let hull_prefixes = (p: list(string)): list(list(string)) => {
  let rec go = (acc, pre, rest) =>
    switch (rest) {
    | [] => acc
    | [x, ...tl] => go([pre @ [x], ...acc], pre @ [x], tl)
    };
  go([], [], p);
};

let hull_is_prefix = (pre: list(string), p: list(string)): bool => {
  let rec go = (a, b) =>
    switch (a, b) {
    | ([], _) => true
    | ([x, ...at], [y, ...bt]) => x == y && go(at, bt)
    | (_, []) => false
    };
  go(pre, p);
};

let hull_former_key = (path: list(string)): string =>
  "{}@" ++ String.concat(".", path);

let hull_paths_of = (lay: CanvasLayout.t): list(list(string)) =>
  List.concat_map(
    (nl: CanvasLayout.node_layout) => hull_prefixes(nl.node.m_path),
    lay.nodes,
  )
  @ List.concat_map(
      (el: CanvasLayout.edge_layout) => hull_prefixes(el.edge.m_path),
      lay.edges,
    )
  |> List.sort_uniq(compare)
  |> List.sort((a, b) => compare(List.length(a), List.length(b)));

let hull_circles_at =
    (
      lay: CanvasLayout.t,
      hull_paths: list(list(string)),
      path: list(string),
    )
    : list((string, CanvasLayout.pos, float)) => {
  let former_key = hull_former_key(path);
  let vsuffix = "--v" ++ sanitize(String.concat(".", path));
  let former_pos_of = (p: list(string)): option(CanvasLayout.pos) =>
    List.find_opt(
      (nl: CanvasLayout.node_layout) => nl.node.key == hull_former_key(p),
      lay.nodes,
    )
    |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
  /* ancestor copies of descendant circles grow with the depth
     difference so a parent blob always contains its sub-blobs with
     visible padding */
  let depth_pad = (owner: list(string)): float =>
    float_of_int(List.length(owner) - List.length(path)) *. 14.;
  /* a former grows with the depth of the module tree beneath it */
  let subtree_bump = (fp: list(string)): float =>
    14.
    *. float_of_int(
         List.fold_left(
           (m, q) =>
             hull_is_prefix(fp, q)
               ? max(m, List.length(q) - List.length(fp)) : m,
           0,
           hull_paths,
         ),
       );
  let node_circles =
    List.filter_map(
      (nl: CanvasLayout.node_layout) =>
        if (!hull_is_prefix(path, nl.node.m_path)) {
          None;
        } else {
          let exact = nl.node.m_path == path;
          /* ANY former keeps its former-size in ancestor copies too */
          let former_sized =
            String.length(nl.node.key) >= 3
            && String.sub(nl.node.key, 0, 3) == "{}@";
          let r =
            (former_sized ? 62. +. subtree_bump(nl.node.m_path) : 46.)
            +. depth_pad(nl.node.m_path);
          Some((
            "hullc-n-"
            ++ sanitize(nl.node.key)
            ++ (exact || nl.node.key == former_key ? "" : vsuffix),
            nl.p,
            r,
          ));
        },
      lay.nodes,
    );
  let label_circles =
    List.concat_map(
      (el: CanvasLayout.edge_layout) =>
        if (!hull_is_prefix(path, el.edge.m_path)) {
          [];
        } else {
          let exact = el.edge.m_path == path;
          let pad = depth_pad(el.edge.m_path);
          let en = sanitize(el.edge.e_name);
          let sfx = exact ? "" : vsuffix;
          let lp = el.label_p;
          /* the SAUSAGE along the edge is the pseudopod now */
          ignore(former_pos_of);
          [("hullc-l-" ++ en ++ sfx, lp, 38. +. pad)];
        },
      lay.edges,
    );
  node_circles @ label_circles;
};

/* the full circle enumeration: (id, target pos, radius) across every
   viewed path — jelly targets and the drag applier both consume this */
let hull_targets =
    (lay: CanvasLayout.t): list((string, CanvasLayout.pos, float)) => {
  let paths = hull_paths_of(lay);
  List.concat_map(hull_circles_at(lay, paths), paths);
};

/* member-edge SAUSAGES: the edge curve itself joins the metaball as a
   thick round-capped stroke (the browser computes the capsule; the
   blur+threshold unions it with the circles). Ancestor copies widen
   by the depth difference like circles do. */
let hull_sausage_d = (el: CanvasLayout.edge_layout): string =>
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
  );

let hull_sausages_at =
    (lay: CanvasLayout.t, path: list(string))
    : list((string, string, float)) => {
  let vsuffix = "--v" ++ sanitize(String.concat(".", path));
  List.filter_map(
    (el: CanvasLayout.edge_layout) =>
      if (!hull_is_prefix(path, el.edge.m_path) || el.endo) {
        None;
      } else {
        let exact = el.edge.m_path == path;
        let pad =
          float_of_int(List.length(el.edge.m_path) - List.length(path))
          *. 14.;
        Some((
          "hulls-e-" ++ sanitize(el.edge.e_name) ++ (exact ? "" : vsuffix),
          hull_sausage_d(el),
          52. +. 2. *. pad,
        ));
      },
    lay.edges,
  );
};

let edge_label =
    (
      ~inject_jump: Haz3lcore.Id.t => Effect.t(unit),
      ~focused: option(string),
      ~on_edge_click: CanvasGraph.edge => Effect.t(unit),
      ~on_edge_hover: option(string) => Effect.t(unit)=_ => Effect.Ignore,
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
      Attr.on_mouseenter(_ => on_edge_hover(Some(e.e_name))),
      Attr.on_mouseleave(_ => on_edge_hover(None)),
    ],
    [
      text(e.e_label),
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
      ~on_node_contextmenu:
         (CanvasGraph.tynode, (float, float)) => Effect.t(unit)=(_, _) =>
                                                                    Effect.Ignore,
      ~just_placed: list(string)=[],
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
    Attr.on_contextmenu(evt => {
      open Js_of_ocaml;
      let x = float_of_int(Js.Unsafe.coerce(evt)##.clientX)
      and y = float_of_int(Js.Unsafe.coerce(evt)##.clientY);
      Effect.Many([
        Effect.Prevent_default,
        Effect.Stop_propagation,
        on_node_contextmenu(n, (x, y)),
      ]);
    }),
    clss(["clickable"]),
  ];
  let is_module =
    String.length(n.key) >= 3 && String.sub(n.key, 0, 3) == "{}@";
  let label_nodes =
    if (is_module) {
      [
        /* the implicit module type: {} glyph inside, NAME below like a
           type node */
        span(~attrs=[clss(["canvas-node-glyph"])], [text("{}")]),
        span(~attrs=[clss(["canvas-node-label"])], [text(n.label)]),
      ];
    } else if (n.label == "") {
      [];
    } else {
      [span(~attrs=[clss(["canvas-node-label"])], [text(n.label)])];
    };
  div(
    ~attrs=
      [
        Attr.id(node_dom_id(n.key)),
        clss(
          ["canvas-node", kind_cls(n.kind)]
          @ (is_module ? ["node-module"] : [])
          @ (n.n_err ? ["node-err"] : [])
          /* grows out of the placement-preview dot */
          @ (List.mem(n.key, just_placed) ? ["just-placed"] : []),
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

/* constants as slow-orbiting dots around their type's node: labels only
   on hover/click (the docked label pills cluttered and perturbed
   layout). Deterministic: phase comes from a name hash via a negative
   animation-delay — same program, same sky. */
let value_view =
    (
      ~inject_jump as _,
      ~on_value_click: CanvasGraph.value => Effect.t(unit),
      vl: CanvasLayout.value_layout,
    )
    : Node.t => {
  let v = vl.value;
  let hash =
    String.fold_left(
      (h, c) => (h * 31 + Char.code(c)) mod 997,
      7,
      v.v_name,
    );
  let period = 90.;
  let phase = float_of_int(hash mod 90);
  div(
    ~attrs=[
      Attr.id("cval-" ++ sanitize(v.v_name)),
      clss(["canvas-orbit"]),
      anchor_style(vl.p),
    ],
    [
      div(
        ~attrs=[
          clss(["orbit-arm"]),
          Attr.create(
            "style",
            Printf.sprintf(
              "animation-duration: %.0fs; animation-delay: -%.0fs;",
              period,
              phase,
            ),
          ),
        ],
        [
          div(
            ~attrs=[
              clss(["orbit-dot"] @ (v.v_err ? ["node-err"] : [])),
              Attr.create("style", Printf.sprintf("left: %.1fpx;", vl.vr)),
              Attr.title(v.v_name ++ " : " ++ v.v_ty),
              Attr.on_click(_ => on_value_click(v)),
            ],
            [],
          ),
        ],
      ),
    ],
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
      ~collapsed_counts: list((string, int))=[],
      ~on_hull_toggle: string => Effect.t(unit)=_ => Effect.Ignore,
      ~on_edge_click: CanvasGraph.edge => Effect.t(unit),
      ~on_node_mousedown:
         (
           CanvasGraph.tynode,
           Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.mouseEvent)
         ) =>
         Effect.t(unit),
      ~on_canvas_click: option(((float, float)) => Effect.t(unit))=None,
      /* blank-canvas double-click: create a type node here (modeless) */
      ~on_canvas_dblclick: ((float, float)) => Effect.t(unit)=_ =>
                                                                  Effect.Ignore,
      /* right-click menus: blank canvas gets (model, client) coords, a
         node gets (node, client) */
      ~on_canvas_contextmenu:
         ((float, float), (float, float)) => Effect.t(unit)=(_, _) =>
                                                                 Effect.Ignore,
      ~on_node_contextmenu:
         (CanvasGraph.tynode, (float, float)) => Effect.t(unit)=(_, _) =>
                                                                    Effect.Ignore,
      /* connect mode: preview edges from these source-node centers to
         the cursor */
      ~connect_pts: list(CanvasLayout.pos)=[],
      /* key of a node placed moments ago (grow-in animation) */
      ~just_placed: list(string)=[],
      ~zoom: float=1.,
      /* pane size in layout px: the root grows to fill it so the dot
         field covers the whole visible canvas */
      ~min_size: (float, float)=(0., 0.),
      ~focused: option(string),
      ~avatar: option((CanvasLayout.pos, string)),
      /* streaming reasoning tail shown in a small bubble by the avatar */
      ~avatar_bubble: option(string)=None,
      /* tool name of the beat just shown; briefly replaces the bubble */
      ~avatar_toast: option(string)=None,
      /* hover/focus dependency fan: subdued curves from a function's
         pill to the pills/nodes of the bindings it references */
      ~dep_fan: list((CanvasLayout.pos, CanvasLayout.pos))=[],
      ~on_edge_hover: option(string) => Effect.t(unit)=_ => Effect.Ignore,
      ~on_value_click: option(CanvasGraph.value => Effect.t(unit))=None,
      /* true-background click with NO gesture mode active (clears
         focus panels); never sets the placing cursor */
      ~on_canvas_plain_click: unit => Effect.t(unit)=() => Effect.Ignore,
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
      @ List.mapi(dep_link_svg, lay.dep_links)
      @ List.mapi(formation_svg, lay.formations)
      @ List.concat_map(leader_svg, lay.edges)
      @ List.concat_map(edge_svg(~focused, ~radius_of), lay.edges),
    );
  /* target==currentTarget: only true background events (nodes/labels
     are their own targets; the edges svg is pointer-events: none) */
  /* gestures live on the pan-pad wrapper (which spans the slack ring),
     so placement isn't boxed into the content extent; coordinates are
     always relative to the ROOT's rect, and "background" means the
     pad itself or the root — anything deeper is a node/label */
  let is_bg = (evt): bool => {
    open Js_of_ocaml;
    let tgt = Js.Unsafe.coerce(evt)##.target;
    let cur = Js.Unsafe.coerce(evt)##.currentTarget;
    let same: bool =
      Js.to_bool(
        Js.Unsafe.coerce(
          Js.Unsafe.meth_call(cur, "isSameNode", [|Js.Unsafe.inject(tgt)|]),
        ),
      );
    let is_root: bool =
      Js.to_bool(
        Js.Unsafe.meth_call(
          Js.Unsafe.coerce(tgt)##.classList,
          "contains",
          [|Js.Unsafe.inject(Js.string("canvas-root"))|],
        ),
      );
    same || is_root;
  };
  let root_rect = evt => {
    open Js_of_ocaml;
    let cur = Js.Unsafe.coerce(evt)##.currentTarget;
    let root =
      Js.Unsafe.meth_call(
        cur,
        "querySelector",
        [|Js.Unsafe.inject(Js.string(".canvas-root"))|],
      );
    Js.Unsafe.meth_call(root, "getBoundingClientRect", [||]);
  };
  let bg_event_coords = evt =>
    if (is_bg(evt)) {
      open Js_of_ocaml;
      let rect = root_rect(evt);
      let left: float = Js.Unsafe.coerce(rect)##.left;
      let top: float = Js.Unsafe.coerce(rect)##.top;
      let cx = float_of_int(Js.Unsafe.coerce(evt)##.clientX)
      and cy = float_of_int(Js.Unsafe.coerce(evt)##.clientY);
      Some((((cx -. left) /. zoom, (cy -. top) /. zoom), (cx, cy)));
    } else {
      None;
    };
  let gesture_attrs = [
    Attr.on_click(evt =>
      switch (on_canvas_click, bg_event_coords(evt)) {
      | (None, Some(_)) => on_canvas_plain_click()
      | _ => Effect.Ignore
      }
    ),
    Attr.on_double_click(evt =>
      switch (bg_event_coords(evt)) {
      | Some((model, _)) => on_canvas_dblclick(model)
      | None => Effect.Ignore
      }
    ),
    Attr.on_contextmenu(evt =>
      switch (bg_event_coords(evt)) {
      | Some((model, client)) =>
        Effect.Many([
          Effect.Prevent_default,
          on_canvas_contextmenu(model, client),
        ])
      | None => Effect.Ignore
      }
    ),
  ];
  let bg_attrs =
    switch (on_canvas_click) {
    | Some(f) => [
        Attr.on_mousedown(evt
          /* only true background presses (nodes/labels are their own
             targets; the edges svg is pointer-events: none) */
          =>
            switch (bg_event_coords(evt)) {
            | Some((model, _)) => f(model)
            | None => Effect.Ignore
            }
          ),
      ]
    | None => []
    };
  /* gesture telegraphs: while placing, a pulsing dot rides the nearest
     lattice point under the cursor; while connecting, rubber-band lines
     run from each collected source to the cursor. Both update by direct
     DOM mutation on mousemove — no re-render per pointer event. */
  let placing = on_canvas_click != None;
  let track_attrs =
    placing || connect_pts != []
      ? [
        Attr.on_mousemove(evt => {
          open Js_of_ocaml;
          let rect = root_rect(evt);
          let left: float = Js.Unsafe.coerce(rect)##.left;
          let top: float = Js.Unsafe.coerce(rect)##.top;
          let x =
            (float_of_int(Js.Unsafe.coerce(evt)##.clientX) -. left) /. zoom;
          let y =
            (float_of_int(Js.Unsafe.coerce(evt)##.clientY) -. top) /. zoom;
          switch (Util.JsUtil.get_elem_by_id_opt("place-preview")) {
          | Some(el) =>
            let st = Js.Unsafe.coerce(el)##.style;
            st##.left :=
              Js.string(Printf.sprintf("%.1fpx", CanvasLayout.snap(x)));
            st##.top :=
              Js.string(Printf.sprintf("%.1fpx", CanvasLayout.snap(y)));
          | None => ()
          };
          if (connect_pts != []) {
            let set_d = (id, d) =>
              switch (Util.JsUtil.get_elem_by_id_opt(id)) {
              | Some(el) =>
                ignore(
                  Js.Unsafe.meth_call(
                    el,
                    "setAttribute",
                    [|
                      Js.Unsafe.inject(Js.string("d")),
                      Js.Unsafe.inject(Js.string(d)),
                    |],
                  ),
                )
              | None => ()
              };
            /* edge-style horizontal-tangent cubic */
            let edge_d = (a: CanvasLayout.pos, bx, by) => {
              let k = max(30., 0.4 *. Float.abs(bx -. a.x));
              Printf.sprintf(
                "M %.1f,%.1f C %.1f,%.1f %.1f,%.1f %.1f,%.1f",
                a.x,
                a.y,
                a.x +. k,
                a.y,
                bx -. k,
                by,
                bx,
                by,
              );
            };
            switch (connect_pts) {
            | [p] => set_d("connect-main", edge_d(p, x, y))
            | ps =>
              let n = float_of_int(List.length(ps));
              let cx =
                List.fold_left((a, p: CanvasLayout.pos) => a +. p.x, 0., ps)
                /. n
              and cy =
                List.fold_left((a, p: CanvasLayout.pos) => a +. p.y, 0., ps)
                /. n;
              /* predicted product pin: matches the commit's weighting */
              let px = (2. *. cx +. x) /. 3.
              and py = (2. *. cy +. y) /. 3.;
              List.iteri(
                (i, p: CanvasLayout.pos) => {
                  let mx = (p.x +. px) /. 2.;
                  set_d(
                    "connect-form-" ++ string_of_int(i),
                    Printf.sprintf(
                      "M %.1f,%.1f C %.1f,%.1f %.1f,%.1f %.1f,%.1f",
                      p.x,
                      p.y,
                      mx,
                      p.y,
                      mx,
                      py,
                      px,
                      py,
                    ),
                  );
                },
                ps,
              );
              set_d(
                "connect-main",
                edge_d(
                  {
                    x: px,
                    y: py,
                  },
                  x,
                  y,
                ),
              );
            };
          };
          Effect.Ignore;
        }),
      ]
      : [];
  /* both containers are ALWAYS in the children list (hidden/empty when
     idle): conditional presence displaced later un-keyed siblings in
     the vdom diff, recreating every node element per mode toggle */
  /* the preview draws what WILL be drawn: formation curves from each
     source into the predicted product point (2/3 sources centroid + 1/3
     cursor — the pin the commit will use), and a main-style edge curve
     from there to the cursor. Single source: just the edge curve. */
  let telegraph_nodes = [
    div(
      ~attrs=[
        Attr.id("place-preview"),
        clss(["place-preview"] @ (placing ? [] : ["idle"])),
        Attr.create("style", "left: -100px; top: -100px;"),
      ],
      [],
    ),
    svg(
      "svg",
      [
        clss(["connect-preview"]),
        Attr.create("width", fmt(lay.width)),
        Attr.create("height", fmt(lay.height)),
      ],
      List.mapi(
        (i, p: CanvasLayout.pos) =>
          svg(
            "path",
            [
              Attr.id("connect-form-" ++ string_of_int(i)),
              clss(["connect-form"]),
              Attr.create(
                "d",
                Printf.sprintf("M %s,%s", fmt(p.x), fmt(p.y)),
              ),
            ],
            [],
          ),
        List.length(connect_pts) > 1 ? connect_pts : [],
      )
      @ (
        connect_pts == []
          ? []
          : [
            svg(
              "path",
              [
                Attr.id("connect-main"),
                clss(["connect-main"]),
                Attr.create("d", "M -10,-10"),
                Attr.create("marker-end", "url(#cnv-arrow)"),
              ],
              [],
            ),
          ]
      ),
    ),
  ];
  div(
    ~attrs=
      [
        clss(
          ["canvas-pan-pad"] @ (on_canvas_click == None ? [] : ["placing"]),
        ),
      ]
      @ bg_attrs
      @ gesture_attrs
      @ track_attrs,
    [
      div(
        ~attrs=[
          clss(["canvas-root"]),
          Attr.create(
            "style",
            {
              let (mw, mh) = min_size;
              Printf.sprintf(
                "width: %spx; height: %spx; zoom: %s;",
                fmt(max(lay.width, mw)),
                fmt(max(lay.height, mh)),
                /* zoom needs full precision: %.1f rounds a pane-fitting
                   zoom up and the min_size-floored root overflows by the
                   excess, re-summoning the scrollbars fit just removed */
                Printf.sprintf("%.4f", zoom),
              );
            },
          ),
        ],
        [
          {
            /* module hulls: per root module, a metaball union (SVG
               blur + alpha threshold) of circles at the former, every
               internal node, and every member-function label — the
               perimeter contains all internals; only external
               connections cross it */
            let hull_paths = hull_paths_of(lay);
            let hull_circles_at = hull_circles_at(lay, hull_paths);
            let former_key_of = hull_former_key;
            let hull_layer =
              Node.create_svg(
                "svg",
                ~attrs=[
                  clss(["canvas-hulls"]),
                  Attr.create("width", fmt(lay.width)),
                  Attr.create("height", fmt(lay.height)),
                ],
                [
                  Node.create_svg(
                    "defs",
                    [
                      Node.create_svg(
                        "filter",
                        ~attrs=[
                          Attr.id("metaball"),
                          Attr.create("x", "-60%"),
                          Attr.create("y", "-60%"),
                          Attr.create("width", "220%"),
                          Attr.create("height", "220%"),
                        ],
                        [
                          Node.create_svg(
                            "feGaussianBlur",
                            ~attrs=[
                              Attr.create("in", "SourceGraphic"),
                              Attr.create("stdDeviation", "15"),
                            ],
                            [],
                          ),
                          Node.create_svg(
                            "feColorMatrix",
                            ~attrs=[
                              Attr.create("mode", "matrix"),
                              Attr.create(
                                "values",
                                "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 48 -23",
                              ),
                            ],
                            [],
                          ),
                        ],
                      ),
                    ],
                  ),
                  ...List.concat_map(
                       path => {
                         let circles = hull_circles_at(path);
                         let depth = List.length(path);
                         let root =
                           switch (path) {
                           | [r, ..._] => r
                           | [] => ""
                           };
                         let former =
                           List.find_opt(
                             (nl: CanvasLayout.node_layout) =>
                               nl.node.key == former_key_of(path),
                             lay.nodes,
                           );
                         /* module palette by first appearance: cyan,
                            magenta, then friends; nested paths share
                            their root's color (depth = opacity) */
                         let palette = [|
                           "#6fbfc9",
                           "#d493c6",
                           "#d8b56a",
                           "#97a5e0",
                           "#a4c48d",
                           "#e0a186",
                         |];
                         let color = {
                           let roots =
                             List.filter_map(
                               fun
                               | [r] => Some(r)
                               | _ => None,
                               hull_paths,
                             );
                           let rec idx = (i, l) =>
                             switch (l) {
                             | [] => 0
                             | [x, ..._] when x == root => i
                             | [_, ...tl] => idx(i + 1, tl)
                             };
                           palette[idx(0, roots) mod Array.length(palette)];
                         };
                         [
                           Node.create_svg(
                             "g",
                             ~attrs=[
                               clss([
                                 "canvas-hull",
                                 Printf.sprintf(
                                   "hull-depth-%d",
                                   min(depth, 3),
                                 ),
                               ]),
                               Attr.create(
                                 "style",
                                 "filter: url(#metaball); fill: "
                                 ++ color
                                 ++ "; stroke: "
                                 ++ color
                                 ++ ";",
                               ),
                             ],
                             List.map(
                               ((id, d, w)) =>
                                 Node.create_svg(
                                   "path",
                                   ~attrs=[
                                     Attr.id(id),
                                     Attr.create("d", d),
                                     Attr.create("fill", "none"),
                                     Attr.create("stroke-width", fmt(w)),
                                     Attr.create("stroke-linecap", "round"),
                                   ],
                                   [],
                                 ),
                               hull_sausages_at(lay, path),
                             )
                             @ List.map(
                                 ((id, p: CanvasLayout.pos, r)) =>
                                   Node.create_svg(
                                     "circle",
                                     ~attrs=[
                                       Attr.id(id),
                                       Attr.create("cx", fmt(p.x)),
                                       Attr.create("cy", fmt(p.y)),
                                       Attr.create("r", fmt(r)),
                                     ],
                                     [],
                                   ),
                                 circles,
                               ),
                           ),
                         ]
                         @ (
                           switch (former, depth) {
                           | (Some(nl), 1) =>
                             let count =
                               List.assoc_opt(root, collapsed_counts);
                             /* the node carries the module name now;
                                expanded hulls show only a small
                                collapse affordance */
                             let label =
                               switch (count) {
                               | Some(n) =>
                                 Printf.sprintf(
                                   "%s (%d) \xe2\x96\xb8",
                                   root,
                                   n,
                                 )
                               | None => "▾"
                               };
                             [
                               Node.create_svg(
                                 "text",
                                 ~attrs=[
                                   clss(
                                     ["canvas-hull-label"]
                                     @ (
                                       count == None ? [] : ["hull-collapsed"]
                                     ),
                                   ),
                                   Attr.create("x", fmt(nl.p.x)),
                                   Attr.create("y", fmt(nl.p.y -. 62.)),
                                   Attr.create("text-anchor", "middle"),
                                   Attr.on_click(_ => on_hull_toggle(root)),
                                 ],
                                 [text(label)],
                               ),
                             ];
                           | _ => []
                           }
                         );
                       },
                       hull_paths,
                     ),
                ],
              );
            /* the under-layer: call/constant dependencies of the hovered
               or focused function, drawn beneath everything semantic */
            let fmt' = fmt;
            let depfan_svg =
              Node.create_svg(
                "svg",
                ~attrs=[
                  clss(["canvas-depfan"]),
                  Attr.create("width", fmt'(lay.width)),
                  Attr.create("height", fmt'(lay.height)),
                ],
                List.map(
                  ((a: CanvasLayout.pos, b: CanvasLayout.pos)) => {
                    let mx = (a.x +. b.x) /. 2.
                    and my = (a.y +. b.y) /. 2.;
                    let dx = b.x -. a.x
                    and dy = b.y -. a.y;
                    let d = max(1., Float.hypot(dx, dy));
                    /* slight perpendicular sag so fans don't overlap edges */
                    let cx = mx -. dy /. d *. 14.
                    and cy = my +. dx /. d *. 14.;
                    Node.create_svg(
                      "path",
                      ~attrs=[
                        Attr.create(
                          "d",
                          Printf.sprintf(
                            "M %s %s Q %s %s %s %s",
                            fmt'(a.x),
                            fmt'(a.y),
                            fmt'(cx),
                            fmt'(cy),
                            fmt'(b.x),
                            fmt'(b.y),
                          ),
                        ),
                      ],
                      [],
                    );
                  },
                  dep_fan,
                ),
              );
            div(
              ~attrs=[clss(["canvas-underlayers"])],
              [hull_layer, depfan_svg],
            );
          },
          edges_svg,
        ]
        @ telegraph_nodes
        @ List.map(
            node_view(~on_node_mousedown, ~on_node_contextmenu, ~just_placed),
            lay.nodes,
          )
        @ List.map(
            edge_label(
              ~inject_jump,
              ~focused,
              ~on_edge_click,
              ~on_edge_hover,
            ),
            lay.edges,
          )
        @ {
          let ovc =
            switch (on_value_click) {
            | Some(f) => f
            | None => ((v: CanvasGraph.value) => inject_jump(v.v_id))
            };
          List.map(
            value_view(~inject_jump, ~on_value_click=ovc),
            lay.values,
          );
        }
        @ (
          switch (avatar) {
          | Some((p, _) as a) =>
            [avatar_view(a)]
            /* the bubble/toast anchor to the avatar ICON (which sits at
               p + (14, -34)), not the node; flip sides when the icon is
               too close to the top or right edge of the board */
            @ {
              let ax = p.x +. 14.
              and ay = p.y -. 34.;
              let near_top = ay -. 76. < 4.;
              let near_right = ax +. 220. > lay.width -. 4.;
              let left = near_right ? ax -. 14. : ax +. 30.;
              let top = near_top ? ay +. 30. : ay -. 40.;
              let flips =
                (near_right ? ["b-left"] : [])
                @ (near_top ? ["b-below"] : []);
              let place =
                Attr.create(
                  "style",
                  Printf.sprintf(
                    "left: %spx; top: %spx;",
                    fmt(left),
                    fmt(top),
                  ),
                );
              switch (avatar_toast, avatar_bubble) {
              | (Some(name), _) => [
                  /* just-landed tool call: brief action chip in the
                     bubble's spot */
                  div(
                    ~attrs=[clss(["canvas-avatar-toast"] @ flips), place],
                    [text(name)],
                  ),
                ]
              | (None, Some(txt)) => [
                  div(
                    ~attrs=[clss(["canvas-avatar-bubble"] @ flips), place],
                    [
                      div(~attrs=[clss(["bubble-trail", "t1"])], []),
                      div(~attrs=[clss(["bubble-trail", "t2"])], []),
                      div(
                        ~attrs=[clss(["bubble-cloud"])],
                        [
                          div(
                            ~attrs=[clss(["bubble-ticker"])],
                            [span([text(txt)])],
                          ),
                        ],
                      ),
                    ],
                  ),
                ]
              | (None, None) => []
              };
            }
          | None => []
          }
        ),
      ),
    ],
  );
};
