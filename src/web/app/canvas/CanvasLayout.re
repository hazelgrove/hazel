/* CanvasLayout — deterministic layered layout for CanvasGraph.
   Grid nodes (aliases, derived, ghosts, and feed-forward products) get
   left-to-right layers by type-dependency; satellite nodes dock beside an
   anchor: per-use builtin terminals beside their consumer, and "loop"
   products — tuple inputs that feed an edge back into one of their own
   components (the MVU update shape) — beside that component, so feedback
   loops stay compact instead of stretching the whole graph. Edges are
   horizontal-tangent cubic béziers; single-argument endofunctions render
   as orbits. Motion between layouts is smoothed by the id-keyed FLIP in
   Animation.re, so this recomputes from scratch. */

type pos = {
  x: float,
  y: float,
};

type node_layout = {
  node: CanvasGraph.tynode,
  p: pos,
  r: float,
};

type edge_layout = {
  edge: CanvasGraph.edge,
  src_p: pos,
  dst_p: pos,
  c1: pos, /* bézier control points (horizontal tangents) */
  c2: pos,
  endo: bool, /* src == dst: render as orbit */
  orbit_rank: int, /* stacking index among orbits on the same node */
  label_p: pos,
};

type value_layout = {
  value: CanvasGraph.value,
  p: pos,
};

type t = {
  nodes: list(node_layout),
  edges: list(edge_layout),
  values: list(value_layout),
  formations: list((pos, pos)), /* component → product feed lines */
  dep_links: list((pos, pos)), /* alias-body dependency hints */
  width: float,
  height: float,
  /* the normalization translation: final = pre-norm + origin. Click
     placements convert final coords back to the pre-norm frame with it. */
  origin: pos,
};

let col_w = 190.;
let row_h = 120.;
let margin = 70.;

let base_radius = 20.;

let node_radius = (~fan: int, n: CanvasGraph.tynode): float => {
  let ctr_bump = float_of_int(min(List.length(n.ctrs), 4)) *. 2.
  and fan_bump = float_of_int(min(fan, 6)) *. 2.5;
  switch (n.kind) {
  | Builtin => 11.
  | Ghost => 14.
  | Derived => 15.
  | Product => 10.
  | Alias => base_radius +. ctr_bump +. fan_bump
  };
};

let strip_brackets = (k: string): option(string) =>
  if (String.length(k) >= 2 && k.[0] == '[') {
    Some(String.sub(k, 1, String.length(k) - 2));
  } else {
    None;
  };

let cubic_mid = (s: pos, c1: pos, c2: pos, d: pos): pos => {
  x: 0.125 *. s.x +. 0.375 *. c1.x +. 0.375 *. c2.x +. 0.125 *. d.x,
  y: 0.125 *. s.y +. 0.375 *. c1.y +. 0.375 *. c2.y +. 0.125 *. d.y,
};

/* unit vector a→b */
let norm = (a: pos, b: pos): pos => {
  let dx = b.x -. a.x
  and dy = b.y -. a.y;
  let l = max(1., sqrt(dx *. dx +. dy *. dy));
  {
    x: dx /. l,
    y: dy /. l,
  };
};

/* the point [d] along the way from [a] toward [b] */
let offset_along = (a: pos, b: pos, d: float): pos => {
  let u = norm(a, b);
  {
    x: a.x +. u.x *. d,
    y: a.y +. u.y *. d,
  };
};

/* How a non-grid node docks to its anchor. */
type dock =
  | DockIn /* builtin terminal on the input side */
  | DockOut /* builtin terminal on the output side */
  | DockLoop /* loop product: above-left of its result component */
  | DockDeriv; /* derived [T]: beneath its element type */

/* ~x_scale stretches grid columns so a small graph fills the available
   panel width; dock offsets stay fixed (satellite distances shouldn't
   stretch), and all edge/rim geometry derives from final positions.
   ~offsets are user drag deltas keyed by node key, applied after auto
   placement (edges/rims then derive from the moved positions, and the
   final normalization translates deltas and auto positions together). */
let layout =
    (
      ~x_scale=1.,
      ~offsets: list((string, (float, float)))=[],
      ~pins: list((string, (float, float)))=[],
      g: CanvasGraph.t,
    )
    : t => {
  /* ---- classify: grid vs docked ---- */
  let is_loop_product = (n: CanvasGraph.tynode): bool =>
    n.kind == Product
    && List.exists(
         (e: CanvasGraph.edge) =>
           e.e_src == n.key && List.mem(e.dst, n.parts),
         g.edges,
       );
  let loop_anchor = (n: CanvasGraph.tynode): string =>
    switch (
      List.find_opt(
        (e: CanvasGraph.edge) =>
          e.e_src == n.key && List.mem(e.dst, n.parts),
        g.edges,
      )
    ) {
    | Some(e) => e.dst
    | None => ""
    };
  let docked: list((CanvasGraph.tynode, string, dock)) =
    List.filter_map(
      (n: CanvasGraph.tynode) =>
        switch (n.sat) {
        | Some((anchor, output)) =>
          Some((n, anchor, output ? DockOut : DockIn))
        | None when is_loop_product(n) =>
          Some((n, loop_anchor(n), DockLoop))
        | None =>
          /* derived [T] docks beneath T when T is itself on the grid */
          switch (n.kind, strip_brackets(n.key)) {
          | (Derived, Some(ik)) =>
            switch (
              List.find_opt((m: CanvasGraph.tynode) => m.key == ik, g.nodes)
            ) {
            | Some(m) when m.sat == None && !is_loop_product(m) =>
              Some((n, ik, DockDeriv))
            | _ => None
            }
          | _ => None
          }
        },
      g.nodes,
    );
  let docked_keys =
    List.map(((n: CanvasGraph.tynode, _, _)) => n.key, docked);
  let grid_nodes =
    List.filter(
      (n: CanvasGraph.tynode) => !List.mem(n.key, docked_keys),
      g.nodes,
    );
  let grid_keys = List.map((n: CanvasGraph.tynode) => n.key, grid_nodes);
  let find_grid = (k: string): option(CanvasGraph.tynode) =>
    List.find_opt((n: CanvasGraph.tynode) => n.key == k, grid_nodes);

  /* ---- layer assignment (grid only) ---- */
  let tbl: Hashtbl.t(string, int) = Hashtbl.create(16);
  let rec layer_of = (~depth=0, k: string): int =>
    switch (Hashtbl.find_opt(tbl, k)) {
    | Some(l) => l
    | None when depth > 32 => 0
    | None =>
      Hashtbl.replace(tbl, k, 0); /* cycle guard (Rec types) */
      let l =
        switch (find_grid(k)) {
        | None => 0
        | Some(n) =>
          let dep_layers =
            n.deps
            |> List.filter(d => d != k && List.mem(d, grid_keys))
            |> List.map(layer_of(~depth=depth + 1));
          let inner =
            switch (strip_brackets(k)) {
            | Some(ik) when List.mem(ik, grid_keys) && ik != k => [
                layer_of(~depth=depth + 1, ik),
              ]
            | _ => []
            };
          switch (dep_layers @ inner) {
          | [] => 0
          | ls => List.fold_left(max, 0, ls) + 1
          };
        };
      Hashtbl.replace(tbl, k, l);
      l;
    };
  List.iter(k => ignore(layer_of(k)), grid_keys);
  /* flow constraint: a function's result sits right of its (grid) input */
  for (_ in 1 to 3) {
    List.iter(
      (e: CanvasGraph.edge) =>
        if (e.dst != e.e_src
            && List.mem(e.dst, grid_keys)
            && List.mem(e.e_src, grid_keys)) {
          let src_l = layer_of(e.e_src);
          switch (Hashtbl.find_opt(tbl, e.dst)) {
          | Some(l) when l <= src_l => Hashtbl.replace(tbl, e.dst, src_l + 1)
          | _ => ()
          };
        },
      g.edges,
    );
  };
  /* compact away empty columns */
  let used_layers =
    grid_keys
    |> List.map(k => Option.value(~default=0, Hashtbl.find_opt(tbl, k)))
    |> List.sort_uniq(compare);
  let layer = (k: string): int => {
    let raw = Option.value(~default=0, Hashtbl.find_opt(tbl, k));
    let rec idx = (i, ls) =>
      switch (ls) {
      | [] => 0
      | [l, ..._] when l == raw => i
      | [_, ...rest] => idx(i + 1, rest)
      };
    idx(0, used_layers);
  };

  /* ---- grid slots ---- */
  let max_layer = max(0, List.length(used_layers) - 1);
  let by_layer =
    List.init(max_layer + 1, l =>
      List.filter((n: CanvasGraph.tynode) => layer(n.key) == l, grid_nodes)
    );
  let max_rows =
    List.fold_left((m, ns) => max(m, List.length(ns)), 1, by_layer);
  let fan = (k: string): int =>
    List.length(
      List.filter(
        (e: CanvasGraph.edge) => e.e_src == k || e.dst == k,
        g.edges,
      ),
    );
  let grid_layouts: list(node_layout) =
    List.concat(
      List.mapi(
        (l, ns) => {
          let n_rows = List.length(ns);
          let y0 =
            margin
            +. float_of_int(max_rows - n_rows)
            *. row_h
            /. 2.
            +. row_h
            /. 2.;
          List.mapi(
            (i, n: CanvasGraph.tynode) =>
              {
                node: n,
                p: {
                  x:
                    margin
                    +. (float_of_int(l) *. col_w +. col_w /. 2.)
                    *. x_scale,
                  y: y0 +. float_of_int(i) *. row_h,
                },
                r: node_radius(~fan=fan(n.key), n),
              },
            ns,
          );
        },
        by_layer,
      ),
    );

  /* ---- docked nodes: resolve anchors iteratively (a loop product can
     anchor a builtin terminal of its own) ---- */
  let placed: Hashtbl.t(string, (pos, float)) = Hashtbl.create(16);
  List.iter(
    (nl: node_layout) => Hashtbl.replace(placed, nl.node.key, (nl.p, nl.r)),
    grid_layouts,
  );
  let dock_count: Hashtbl.t((string, dock), int) = Hashtbl.create(8);
  let docked_layouts: ref(list(node_layout)) = ref([]);
  let try_place = ((n: CanvasGraph.tynode, anchor: string, d: dock)): bool =>
    switch (Hashtbl.find_opt(placed, n.key)) {
    | Some(_) => true
    | None =>
      switch (Hashtbl.find_opt(placed, anchor)) {
      | None => false
      | Some((a, ar)) =>
        let i =
          Option.value(
            ~default=0,
            Hashtbl.find_opt(dock_count, (anchor, d)),
          );
        Hashtbl.replace(dock_count, (anchor, d), i + 1);
        let fi = float_of_int(i);
        /* formers ("()"/"[]") tuck close to their alias; labeled builtin
           terminals need the wider berth */
        let dock_dist = n.kind == CanvasGraph.Product ? 46. : 92.;
        let p =
          switch (d) {
          | DockIn => {
              x: a.x -. ar -. dock_dist,
              y: a.y -. 20. +. fi *. 42.,
            }
          | DockOut => {
              x: a.x +. ar +. dock_dist,
              y: a.y -. 20. +. fi *. 42.,
            }
          | DockLoop =>
            /* fan loop products at distinct angles around the anchor so
               several feedback functions stay visually separate */
            let th = (125. +. fi *. 42.) *. Float.pi /. 180.;
            let dist = ar +. 62. +. fi *. 8.;
            {
              x: a.x +. cos(th) *. dist,
              y: a.y -. sin(th) *. dist,
            };
          | DockDeriv => {
              x: a.x,
              y: a.y +. ar +. 52. +. fi *. 36.,
            }
          };
        let r = node_radius(~fan=fan(n.key), n);
        docked_layouts :=
          docked_layouts^
          @ [
            {
              node: n,
              p,
              r,
            },
          ];
        Hashtbl.replace(placed, n.key, (p, r));
        true;
      }
    };
  /* two passes handle anchor chains; anything still unplaced parks at the
     top-left corner rather than vanishing */
  let unresolved =
    docked
    |> List.filter(x => !try_place(x))
    |> List.filter(x => !try_place(x));
  List.iteri(
    (i, (n: CanvasGraph.tynode, _, _)) => {
      let p = {
        x: margin /. 2.,
        y: margin /. 2. +. float_of_int(i) *. 30.,
      };
      let r = node_radius(~fan=fan(n.key), n);
      docked_layouts :=
        docked_layouts^
        @ [
          {
            node: n,
            p,
            r,
          },
        ];
      Hashtbl.replace(placed, n.key, (p, r));
    },
    unresolved,
  );
  /* ---- user drag deltas and click-placement pins ---- */
  let node_layouts =
    List.map(
      (nl: node_layout) =>
        switch (
          List.assoc_opt(nl.node.key, pins),
          List.assoc_opt(nl.node.key, offsets),
        ) {
        | (Some((x, y)), _) => {
            ...nl,
            p: {
              x,
              y,
            },
          }
        | (None, Some((dx, dy))) => {
            ...nl,
            p: {
              x: nl.p.x +. dx,
              y: nl.p.y +. dy,
            },
          }
        | (None, None) => nl
        },
      grid_layouts @ docked_layouts^,
    );
  Hashtbl.reset(placed);
  List.iter(
    (nl: node_layout) => Hashtbl.replace(placed, nl.node.key, (nl.p, nl.r)),
    node_layouts,
  );
  let pos_of = (k: string): option(pos) =>
    Hashtbl.find_opt(placed, k) |> Option.map(fst);
  let radius_of = (k: string): float =>
    Hashtbl.find_opt(placed, k)
    |> Option.map(snd)
    |> Option.value(~default=base_radius);

  /* ---- formation + dependency links (rim-to-rim so arrowheads land) ---- */
  let rim_pair = (from_k: string, to_k: string): option((pos, pos)) =>
    switch (pos_of(from_k), pos_of(to_k)) {
    | (Some(fp), Some(tp)) =>
      Some((
        offset_along(fp, tp, radius_of(from_k) +. 2.),
        offset_along(tp, fp, radius_of(to_k) +. 4.),
      ))
    | _ => None
    };
  let formations =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        switch (n.kind) {
        | Product =>
          (n.parts |> List.filter_map(pk => rim_pair(pk, n.key)))
          /* alias-body formers dock at their alias and feed it */
          @ (
            switch (n.sat) {
            | Some((anchor, _)) => rim_pair(n.key, anchor) |> Option.to_list
            | None => []
            }
          )
        | Derived =>
          /* a docked [T] node forms from its element type */
          switch (strip_brackets(n.key)) {
          | Some(ik)
              when
                List.exists((m: CanvasGraph.tynode) => m.key == ik, g.nodes) =>
            rim_pair(ik, n.key) |> Option.to_list
          | _ => []
          }
        | _ => []
        },
      g.nodes,
    );
  let dep_links =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        switch (n.kind) {
        | Alias =>
          n.deps
          |> List.filter(d => d != n.key && !List.mem(d, n.hidden_deps))
          |> List.filter_map(dk => rim_pair(dk, n.key))
        | _ => []
        },
      g.nodes,
    );

  /* ---- edges ---- */
  let fallback = {
    x: margin,
    y: margin,
  };
  let loop_product_edges: list((string, string)) =
    List.filter_map(
      ((n: CanvasGraph.tynode, anchor, d)) =>
        d == DockLoop ? Some((n.key, anchor)) : None,
      docked,
    );
  let orbit_seen: Hashtbl.t(string, int) = Hashtbl.create(4);
  let edge_layouts =
    List.map(
      (e: CanvasGraph.edge) => {
        let src_p = Option.value(~default=fallback, pos_of(e.e_src));
        let dst_p = Option.value(~default=fallback, pos_of(e.dst));
        let endo = e.e_src == e.dst;
        let is_loop = List.mem((e.e_src, e.dst), loop_product_edges);
        let orbit_rank =
          if (endo) {
            let k =
              Option.value(~default=0, Hashtbl.find_opt(orbit_seen, e.dst));
            Hashtbl.replace(orbit_seen, e.dst, k + 1);
            k;
          } else {
            0;
          };
        if (endo) {
          let orbit_r =
            radius_of(e.dst) +. 16. +. float_of_int(orbit_rank) *. 15.;
          let apex = {
            x: dst_p.x,
            y: dst_p.y -. orbit_r -. 10.,
          };
          {
            edge: e,
            src_p,
            dst_p,
            c1: apex,
            c2: apex,
            endo,
            orbit_rank,
            label_p: {
              x: apex.x,
              y: apex.y -. 2.,
            },
          };
        } else if (is_loop) {
          /* feedback: arc from the product back into its own component,
             bulging perpendicular to the dock axis; label rides the arc */
          let u = norm(dst_p, src_p);
          let v0 = {
            x: -. u.y,
            y: u.x,
          };
          let v =
            v0.y > 0.
              ? {
                x: -. v0.x,
                y: -. v0.y,
              }
              : v0;
          let mid = {
            x: (src_p.x +. dst_p.x) /. 2.,
            y: (src_p.y +. dst_p.y) /. 2.,
          };
          let ctrl = {
            x: mid.x +. v.x *. 38.,
            y: mid.y +. v.y *. 38.,
          };
          let s = offset_along(src_p, ctrl, radius_of(e.e_src) +. 2.);
          let d = offset_along(dst_p, ctrl, radius_of(e.dst) +. 6.);
          {
            edge: e,
            src_p: s,
            dst_p: d,
            c1: ctrl,
            c2: ctrl,
            endo,
            orbit_rank,
            label_p: {
              x: ctrl.x +. v.x *. 14.,
              y: ctrl.y +. v.y *. 14. -. 4.,
            },
          };
        } else {
          let sign = dst_p.x >= src_p.x ? 1. : (-1.);
          let s = {
            x: src_p.x +. sign *. (radius_of(e.e_src) +. 2.),
            y: src_p.y,
          };
          let d = {
            x: dst_p.x -. sign *. (radius_of(e.dst) +. 6.),
            y: dst_p.y,
          };
          let bend = max(24., min(90., abs_float(d.x -. s.x) *. 0.5));
          let c1 = {
            x: s.x +. sign *. bend,
            y: s.y,
          };
          let c2 = {
            x: d.x -. sign *. bend,
            y: d.y,
          };
          {
            edge: e,
            src_p: s,
            dst_p: d,
            c1,
            c2,
            endo,
            orbit_rank,
            label_p: {
              let m = cubic_mid(s, c1, c2, d);
              {
                x: m.x,
                y: m.y -. 6.,
              };
            },
          };
        };
      },
      g.edges,
    );

  /* ---- greedy label separation: nudge colliding chips downward ---- */
  let edge_layouts = {
    let placed_labels: ref(list(pos)) = ref([]);
    List.map(
      (el: edge_layout) => {
        let collides = (p: pos): bool =>
          List.exists(
            q => abs_float(q.x -. p.x) < 58. && abs_float(q.y -. p.y) < 16.,
            placed_labels^,
          );
        let rec free = (p: pos, tries: int): pos =>
          tries > 6 || !collides(p)
            ? p
            : free(
                {
                  x: p.x,
                  y: p.y +. 18.,
                },
                tries + 1,
              );
        let p = free(el.label_p, 0);
        placed_labels := [p, ...placed_labels^];
        {
          ...el,
          label_p: p,
        };
      },
      edge_layouts,
    );
  };

  /* ---- values dock below their type's node ---- */
  let value_layouts =
    List.mapi(
      (i, v: CanvasGraph.value) => {
        let anchor = Option.value(~default=fallback, pos_of(v.v_key));
        {
          value: v,
          p: {
            x: anchor.x -. radius_of(v.v_key) -. 8.,
            y:
              anchor.y
              +. radius_of(v.v_key)
              +. 16.
              +. float_of_int(i mod 2)
              *. 14.,
          },
        };
      },
      g.values,
    );

  /* ---- normalize: translate the content bounding box (whatever actually
     hangs furthest — satellites, labels, values) to a snug uniform pad, so
     the internal grid margins never show up as whitespace ---- */
  let pad = 26.;
  let (min_x, min_y, max_x, max_y) = {
    let b =
      List.fold_left(
        ((x0, y0, x1, y1), nl: node_layout) =>
          (
            min(x0, nl.p.x -. nl.r),
            min(y0, nl.p.y -. nl.r),
            max(x1, nl.p.x +. nl.r),
            max(y1, nl.p.y +. nl.r +. 16.) /* name label below the node */
          ),
        (infinity, infinity, neg_infinity, neg_infinity),
        node_layouts,
      );
    let b =
      List.fold_left(
        ((x0, y0, x1, y1), el: edge_layout) =>
          (
            min(x0, el.label_p.x -. 45.),
            min(y0, el.label_p.y -. 18.),
            max(x1, el.label_p.x +. 45.),
            max(y1, el.label_p.y +. 4.),
          ),
        b,
        edge_layouts,
      );
    List.fold_left(
      ((x0, y0, x1, y1), vl: value_layout) =>
        (
          min(x0, vl.p.x -. 70.),
          min(y0, vl.p.y -. 8.),
          max(x1, vl.p.x),
          max(y1, vl.p.y +. 8.),
        ),
      b,
      value_layouts,
    );
  };
  if (node_layouts == []) {
    {
      nodes: [],
      edges: edge_layouts,
      values: value_layouts,
      formations,
      dep_links,
      width: 2. *. pad,
      height: 2. *. pad,
      origin: {
        x: 0.,
        y: 0.,
      },
    };
  } else {
    let dx = pad -. min_x
    and dy = pad -. min_y;
    let sh = (p: pos): pos => {
      x: p.x +. dx,
      y: p.y +. dy,
    };
    {
      nodes:
        List.map(
          (nl: node_layout) =>
            {
              ...nl,
              p: sh(nl.p),
            },
          node_layouts,
        ),
      edges:
        List.map(
          (el: edge_layout) =>
            {
              ...el,
              src_p: sh(el.src_p),
              dst_p: sh(el.dst_p),
              c1: sh(el.c1),
              c2: sh(el.c2),
              label_p: sh(el.label_p),
            },
          edge_layouts,
        ),
      values:
        List.map(
          (vl: value_layout) =>
            {
              ...vl,
              p: sh(vl.p),
            },
          value_layouts,
        ),
      formations: List.map(((a, b)) => (sh(a), sh(b)), formations),
      dep_links: List.map(((a, b)) => (sh(a), sh(b)), dep_links),
      width: max_x -. min_x +. 2. *. pad,
      height: max_y -. min_y +. 2. *. pad,
      origin: {
        x: dx,
        y: dy,
      },
    };
  };
};
