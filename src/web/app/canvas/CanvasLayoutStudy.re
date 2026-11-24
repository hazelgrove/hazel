/* Read-only comparisons on the exact same extracted graph. No view setting
   or saved position is mutated by this notebook/export facility. */
open Poly;

module G = CanvasGraph;
module L = CanvasLayout;
let components = (g: G.t) => {
  let keys = List.map(~f=(n: G.tynode) => n.key, g.nodes);
  let links =
    List.map(~f=(e: G.edge) => (e.e_src, e.dst), g.edges)
    @ List.concat_map(~f=
        (n: G.tynode) =>
          List.map(~f=
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
      List.concat_map(~f=
        ((a, b)) =>
          List.mem(group, a, ~equal=Poly.equal) ? [b] : List.mem(group, b, ~equal=Poly.equal) ? [a] : [],
        links,
      )
      |> List.filter(~f=k => List.mem(keys, k, ~equal=Poly.equal));
    let next = List.dedup_and_sort(~compare=compare, group @ more);
    List.length(next) == List.length(group) ? group : grow(next);
  };
  List.fold(keys, ~init=[], ~f=(groups, k) =>
      List.exists(~f=g => List.mem(g, k, ~equal=Poly.equal), groups)
        ? groups : groups @ [grow([k])]);
};
let subgraph = (g: G.t, keys) => {
  ...g,
  nodes: List.filter(~f=(n: G.tynode) => List.mem(keys, n.key, ~equal=Poly.equal), g.nodes),
  edges:
    List.filter(~f=
      (e: G.edge) => List.mem(keys, e.e_src, ~equal=Poly.equal) && List.mem(keys, e.dst, ~equal=Poly.equal),
      g.edges,
    ),
  values: List.filter(~f=(v: G.value) => List.mem(keys, v.v_key, ~equal=Poly.equal), g.values),
};
let translate = (dx, dy, l: L.t): L.t => {
  let p = (p: L.pos): L.pos => {
    x: p.x +. dx,
    y: p.y +. dy,
  };
  let links = List.map(~f=((a, b, s, d)) => (a, b, p(s), p(d)));
  {
    ...l,
    nodes:
      List.map(~f=
        (n: L.node_layout) =>
          {
            ...n,
            p: p(n.p),
          },
        l.nodes,
      ),
    edges:
      List.map(~f=
        (e: L.edge_layout) =>
          {
            ...e,
            src_p: p(e.src_p),
            dst_p: p(e.dst_p),
            c1: p(e.c1),
            c2: p(e.c2),
            wire: List.map(~f=p, e.wire),
            label_p: p(e.label_p),
            label_anchor: p(e.label_anchor),
          },
        l.edges,
      ),
    values:
      List.map(~f=
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
    components(g) |> List.map(~f=keys => L.layout_impl(subgraph(g, keys)));
  let area =
    List.fold(blocks, ~init=0., ~f=(a, l: L.t) => a +. l.width *. l.height);
  let target = max(600., Float.sqrt(area *. 1.5));
  let x = ref(0.)
  and y = ref(0.)
  and height = ref(0.)
  and width = ref(0.);
  let placed =
    List.map(~f=
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
    nodes: List.concat_map(~f=(l: t) => l.nodes, placed),
    edges: List.concat_map(~f=(l: t) => l.edges, placed),
    values: List.concat_map(~f=(l: t) => l.values, placed),
    formations: List.concat_map(~f=(l: t) => l.formations, placed),
    dep_links: List.concat_map(~f=(l: t) => l.dep_links, placed),
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
        List.map(~f=
          (n: L.node_layout) =>
            `Assoc([
              ("key", `String(n.node.key)),
              ("label", `String(n.node.label)),
              ("module", `List(List.map(~f=s => `String(s), n.node.m_path))),
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
        List.map(~f=
          (e: L.edge_layout) =>
            `Assoc([
              ("name", `String(e.edge.e_name)),
              ("label", `String(e.edge.e_label)),
              ("src", `String(e.edge.e_src)),
              ("dst", `String(e.edge.dst)),
              ("s", p(e.src_p)),
              ("d", p(e.dst_p)),
              ("wire", `List(List.map(~f=p, e.wire))),
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
        List.map(~f=
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
