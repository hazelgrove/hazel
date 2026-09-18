/* Read-only comparisons on the exact same extracted graph. No view setting
   or saved position is mutated by this notebook/export facility. */
module G = CanvasGraph;
module L = CanvasLayout;
let components = (g: G.t) => {
  let keys = List.map((n: G.tynode) => n.key, g.nodes);
  let links =
    List.map((e: G.edge) => (e.e_src, e.dst), g.edges)
    @ List.concat_map(
        (n: G.tynode) =>
          List.map(
            k => (n.key, k),
            n.deps
            @ n.parts
            @ (
              switch (n.sat) {
              | Some((host, _)) => [host]
              | None => []
              }
            )
            @ (
              switch (n.m_path) {
              | [root, ..._] => ["{}@" ++ root]
              | [] => []
              }
            ),
          ),
        g.nodes,
      );
  let rec grow = group => {
    let more =
      List.concat_map(
        ((a, b)) =>
          List.mem(a, group) ? [b] : List.mem(b, group) ? [a] : [],
        links,
      )
      |> List.filter(k => List.mem(k, keys));
    let next = List.sort_uniq(compare, group @ more);
    List.length(next) == List.length(group) ? group : grow(next);
  };
  List.fold_left(
    (groups, k) =>
      List.exists(List.mem(k), groups) ? groups : groups @ [grow([k])],
    [],
    keys,
  );
};
let subgraph = (g: G.t, keys) => {
  ...g,
  nodes: List.filter((n: G.tynode) => List.mem(n.key, keys), g.nodes),
  edges:
    List.filter(
      (e: G.edge) => List.mem(e.e_src, keys) && List.mem(e.dst, keys),
      g.edges,
    ),
  values: List.filter((v: G.value) => List.mem(v.v_key, keys), g.values),
};
let translate = (dx, dy, l: L.t): L.t => {
  let p = (p: L.pos): L.pos => {
    x: p.x +. dx,
    y: p.y +. dy,
  };
  let links = List.map(((a, b, s, d)) => (a, b, p(s), p(d)));
  {
    ...l,
    nodes:
      List.map(
        (n: L.node_layout) =>
          {
            ...n,
            p: p(n.p),
          },
        l.nodes,
      ),
    edges:
      List.map(
        (e: L.edge_layout) =>
          {
            ...e,
            src_p: p(e.src_p),
            dst_p: p(e.dst_p),
            c1: p(e.c1),
            c2: p(e.c2),
            wire: List.map(p, e.wire),
            label_p: p(e.label_p),
            label_anchor: p(e.label_anchor),
          },
        l.edges,
      ),
    values:
      List.map(
        (v: L.value_layout) =>
          {
            ...v,
            p: p(v.p),
          },
        l.values,
      ),
    formations: links(l.formations),
    dep_links: links(l.dep_links),
  };
};
let packed = (g: G.t) => {
  let blocks =
    components(g) |> List.map(keys => L.layout_impl(subgraph(g, keys)));
  let area =
    List.fold_left((a, l: L.t) => a +. l.width *. l.height, 0., blocks);
  let target = max(600., sqrt(area *. 1.5));
  let x = ref(0.)
  and y = ref(0.)
  and height = ref(0.)
  and width = ref(0.);
  let placed =
    List.map(
      (l: L.t) => {
        if (x^ > 0. && x^ +. l.width > target) {
          y := y^ +. height^ +. 28.;
          x := 0.;
          height := 0.;
        };
        let moved = translate(x^, y^, l);
        width := max(width^, x^ +. l.width);
        height := max(height^, l.height);
        x := x^ +. l.width +. 28.;
        moved;
      },
      blocks,
    );
  L.{
    nodes: List.concat_map((l: t) => l.nodes, placed),
    edges: List.concat_map((l: t) => l.edges, placed),
    values: List.concat_map((l: t) => l.values, placed),
    formations: List.concat_map((l: t) => l.formations, placed),
    dep_links: List.concat_map((l: t) => l.dep_links, placed),
    width: width^,
    height: y^ +. height^,
    origin: {
      x: 0.,
      y: 0.,
    },
  };
};
let json = (l: L.t): Yojson.Safe.t => {
  let p = (p: L.pos) => `List([`Float(p.x), `Float(p.y)]);
  `Assoc([
    ("width", `Float(l.width)),
    ("height", `Float(l.height)),
    (
      "nodes",
      `List(
        List.map(
          (n: L.node_layout) =>
            `Assoc([
              ("key", `String(n.node.key)),
              ("label", `String(n.node.label)),
              ("module", `List(List.map(s => `String(s), n.node.m_path))),
              ("p", p(n.p)),
              ("r", `Float(n.r)),
            ]),
          l.nodes,
        ),
      ),
    ),
    (
      "edges",
      `List(
        List.map(
          (e: L.edge_layout) =>
            `Assoc([
              ("name", `String(e.edge.e_name)),
              ("label", `String(e.edge.e_label)),
              ("src", `String(e.edge.e_src)),
              ("dst", `String(e.edge.dst)),
              ("s", p(e.src_p)),
              ("d", p(e.dst_p)),
              ("wire", `List(List.map(p, e.wire))),
              ("c1", p(e.c1)),
              ("c2", p(e.c2)),
              ("labelAt", p(e.label_p)),
              ("anchor", p(e.label_anchor)),
              ("endo", `Bool(e.endo)),
              ("orbit", `Int(e.orbit_rank)),
            ]),
          l.edges,
        ),
      ),
    ),
    (
      "links",
      `List(
        List.map(
          ((a, b, s, d)) =>
            `Assoc([
              ("src", `String(a)),
              ("dst", `String(b)),
              ("s", p(s)),
              ("d", p(d)),
            ]),
          l.formations @ l.dep_links,
        ),
      ),
    ),
  ]);
};
let compare = (g: G.t) =>
  `Assoc([
    ("combined", json(L.layout_impl(g))),
    ("flow", json(L.layout_impl(~ranking=FunctionFlow, g))),
    ("dependencies", json(L.layout_impl(~ranking=TypeDependencies, g))),
    ("packed", json(packed(g))),
  ]);
