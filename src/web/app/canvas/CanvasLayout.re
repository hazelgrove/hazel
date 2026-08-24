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
  /* the label's natural anchor on its curve; when separation pushed the
     chip away, the view draws a leader line back to this point */
  label_anchor: pos,
};

type value_layout = {
  value: CanvasGraph.value,
  p: pos,
  vr: float /* orbit ring radius */
};

type t = {
  nodes: list(node_layout),
  edges: list(edge_layout),
  values: list(value_layout),
  /* (from_key, to_key, from_rim, to_rim) — keys let the drag follower
     re-anchor these without re-deriving rim geometry */
  formations: list((string, string, pos, pos)), /* component → product */
  dep_links: list((string, string, pos, pos)), /* alias-body dep hints */
  width: float,
  height: float,
  /* the normalization translation: final = pre-norm + origin. Click
     placements convert final coords back to the pre-norm frame with it. */
  origin: pos,
};

let margin = 70.;

/* dot-grid pitch (kept in sync with canvas.css --canvas-grid) */
let grid = 14.;
let snap = (v: float): float => Float.round(v /. grid) *. grid;

let base_radius = 20.;

let node_radius = (~fan: int, n: CanvasGraph.tynode): float => {
  let ctr_bump = float_of_int(min(List.length(n.ctrs), 4)) *. 2.
  and fan_bump = float_of_int(min(fan, 6)) *. 2.5;
  switch (n.kind) {
  | Builtin => 11.
  | Ghost => 14.
  /* hub [T] nodes grow with traffic so their many short edges get
     length to read (orbits and dock rings scale off the radius) */
  | Derived => 15. +. fan_bump
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
  | DockDeriv /* derived [T]: beneath its element type */
  | DockMember; /* module-internal node clustered at its former */

/* ~x_scale stretches grid columns so a small graph fills the available
   panel width; dock offsets stay fixed (satellite distances shouldn't
   stretch), and all edge/rim geometry derives from final positions.
   ~offsets are user drag deltas keyed by node key, applied after auto
   placement (edges/rims then derive from the moved positions, and the
   final normalization translates deltas and auto positions together). */
let layout =
    (
      ~x_scale=1.,
      ~y_scale=1.,
      ~center_within: option(float)=None,
      /* fix the normalization origin (from a no-offsets layout of the
         same graph): user drags must never re-anchor the frame, else
         dragging feeds back into a whole-graph shift */
      ~origin_override: option(pos)=None,
      ~offsets: list((string, (float, float)))=[],
      ~pins: list((string, (float, float)))=[],
      g: CanvasGraph.t,
    )
    : t => {
  /* ---- classify: grid vs docked ---- */
  let fan = (k: string): int =>
    List.length(
      List.filter(
        (e: CanvasGraph.edge) => e.e_src == k || e.dst == k,
        g.edges,
      ),
    );
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
  let is_former = (n: CanvasGraph.tynode): bool =>
    String.length(n.key) >= 3 && String.sub(n.key, 0, 3) == "{}@";
  /* a module's internals (any depth) cluster around the OUTERMOST
     module's former: internal aliases, nested formers — the hull closes
     around the cluster */
  let module_anchor = (n: CanvasGraph.tynode): option(string) =>
    switch (n.m_path) {
    | [] => None
    | [root, ..._] =>
      let top_former = "{}@" ++ root;
      if (is_former(n) && n.key == top_former) {
        None; /* the root former itself hosts the cluster */
      } else {
        Some(top_former);
      };
    };
  let docked: list((CanvasGraph.tynode, string, dock)) =
    List.filter_map(
      (n: CanvasGraph.tynode) =>
        switch (n.sat) {
        | Some((anchor, output)) =>
          Some((n, anchor, output ? DockOut : DockIn))
        | None when is_loop_product(n) =>
          Some((n, loop_anchor(n), DockLoop))
        | None when module_anchor(n) != None =>
          Some((n, Option.get(module_anchor(n)), DockMember))
        | None =>
          /* derived [T] docks beneath T when T is itself on the grid */
          /* only lightly-used [T] tucks beneath its element; a derived
             node with real traffic (orbits, loop products, several fns)
             needs its own grid slot and breathing room */
          switch (n.kind, strip_brackets(n.key)) {
          | (Derived, Some(ik)) when fan(n.key) <= 2 =>
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

  /* ---- placement: delegate to the GraphLayout engine ----
     Rank constraints: alias-body deps (hidden ones included — they still
     order columns), derived [T] after its element, and function flow
     (input strictly left of result). Docked nodes become attachments. */
  let r_of = (n: CanvasGraph.tynode): float =>
    node_radius(~fan=fan(n.key), n);
  let dep_edges =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        n.deps
        |> List.filter(d => d != n.key)
        |> List.map(d =>
             Util.GraphLayout.Spec.{
               src: d,
               dst: n.key,
               ranked: true,
             }
           ),
      grid_nodes,
    );
  let derived_edges =
    List.filter_map(
      (n: CanvasGraph.tynode) =>
        switch (strip_brackets(n.key)) {
        | Some(ik) when ik != n.key =>
          Some(
            Util.GraphLayout.Spec.{
              src: ik,
              dst: n.key,
              ranked: true,
            },
          )
        | _ => None
        },
      grid_nodes,
    );
  let flow_edges =
    List.filter_map(
      (e: CanvasGraph.edge) =>
        e.e_src != e.dst
          ? Some(
              Util.GraphLayout.Spec.{
                src: e.e_src,
                dst: e.dst,
                ranked: true,
              },
            )
          : None,
      g.edges,
    );
  /* legend reservation: a hub's endo-family labels (orbits + loop fns)
     stack directly above it; that column is part of the hub's halo so
     neighbors can never occupy it and the legend never displaces */
  let legend_count = (k: string): int =>
    List.length(
      List.filter(
        (e: CanvasGraph.edge) => e.e_src == k && e.dst == k,
        g.edges,
      ),
    )
    + List.length(
        List.filter(
          ((n: CanvasGraph.tynode, anchor, d)) =>
            d == DockLoop && anchor == k && n.key != "",
          docked,
        ),
      );
  let legend_extent = (k: string): float => {
    let n = legend_count(k);
    n == 0 ? 0. : float_of_int(n) *. 24. +. 28.;
  };
  let attachments =
    List.map(
      ((n: CanvasGraph.tynode, anchor, d)) => {
        let (prefer, dist) =
          switch (d) {
          | DockIn => (
              Util.GraphLayout.Spec.In,
              n.kind == CanvasGraph.Product ? 44. : 72.,
            )
          | DockOut => (Util.GraphLayout.Spec.Out, 72.)
          | DockLoop => (Util.GraphLayout.Spec.In, 64.)
          | DockDeriv => (Util.GraphLayout.Spec.Below, 52.)
          | DockMember => (Util.GraphLayout.Spec.Below, 78.)
          };
        Util.GraphLayout.Spec.{
          id: n.key,
          host: anchor,
          /* labeled terminals hang text below the circle: pad the
             collision radius so neighbors keep clear of the label */
          radius: r_of(n) +. (n.label == "" || n.kind == Product ? 0. : 7.),
          prefer,
          dist,
        };
      },
      docked,
    );
  /* alias-body formers RANK their grid parts before the alias:
     [Todo] -> () -> Model must put [Todo] strictly left of Model, both
     for left-to-right reading and so the two hubs never share a column.
     (Loop products stay unranked — feedback must not stretch columns.) */
  let former_rank_edges =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        switch (n.kind, n.sat) {
        | (Product, Some((alias, _))) =>
          List.map(
            pk =>
              Util.GraphLayout.Spec.{
                src: pk,
                dst: alias,
                ranked: true,
              },
            n.parts,
          )
        | _ => []
        },
      g.nodes,
    );
  /* formation links (component → product) are ordering-only: they pull
     a product's components toward its row without constraining columns */
  let formation_edges =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        n.kind == CanvasGraph.Product
          ? List.map(
              pk =>
                Util.GraphLayout.Spec.{
                  src: pk,
                  dst: n.key,
                  ranked: false,
                },
              n.parts,
            )
          : [],
      g.nodes,
    );
  /* pinned nodes with no spec relationships float FREE of the rank
     grid: a freshly placed node would otherwise claim a row and shove
     every auto-laid node down */
  let all_spec_edges =
    dep_edges
    @ derived_edges
    @ flow_edges
    @ former_rank_edges
    @ formation_edges;
  let spec_touches = (k: string): bool =>
    List.exists(
      (e: Util.GraphLayout.Spec.edge) => e.src == k || e.dst == k,
      all_spec_edges,
    )
    || List.exists(
         (a: Util.GraphLayout.Spec.attachment) => a.host == k,
         attachments,
       );
  let free_keys =
    List.filter_map(
      (n: CanvasGraph.tynode) =>
        List.mem_assoc(n.key, pins) && !spec_touches(n.key)
          ? Some(n.key) : None,
      grid_nodes,
    );
  let res =
    Util.GraphLayout.layout({
      nodes:
        List.filter_map(
          (n: CanvasGraph.tynode) =>
            List.mem(n.key, free_keys)
              ? None
              : Some(
                  Util.GraphLayout.Spec.{
                    id: n.key,
                    radius: r_of(n),
                    extent_above: legend_extent(n.key),
                    extent_below: 0.,
                  },
                ),
          grid_nodes,
        ),
      edges:
        dep_edges
        @ derived_edges
        @ flow_edges
        @ former_rank_edges
        @ formation_edges,
      attachments,
      col_gap: 126.,
      row_gap: 72.,
      margin,
      x_stretch: x_scale,
      y_stretch: y_scale,
      order_sweeps: 4,
    });
  let placed_layouts: list(node_layout) =
    List.filter_map(
      (n: CanvasGraph.tynode) =>
        Util.GraphLayout.pos_of(res, n.key)
        |> Option.map((p: Util.GraphLayout.pos) =>
             {
               node: n,
               p: {
                 x: p.x,
                 y: p.y,
               },
               r: r_of(n),
             }
           ),
      g.nodes,
    )
    /* free-floating pinned nodes: position straight from the pin (the
       pins pass below re-applies the same value) */
    @ List.filter_map(
        (n: CanvasGraph.tynode) =>
          List.mem(n.key, free_keys)
            ? List.assoc_opt(n.key, pins)
              |> Option.map(((x, y)) =>
                   {
                     node: n,
                     p: {
                       x,
                       y,
                     },
                     r: r_of(n),
                   }
                 )
            : None,
        g.nodes,
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
      placed_layouts,
    )
    /* snap-to-grid: node centers land on the dot lattice; drags and
       pins snap too (they pass through here) */
    |> List.map((nl: node_layout) =>
         {
           ...nl,
           p: {
             x: snap(nl.p.x),
             y: snap(nl.p.y),
           },
         }
       );
  let placed: Hashtbl.t(string, (pos, float)) = Hashtbl.create(16);
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
  let rim_pair =
      (from_k: string, to_k: string): option((string, string, pos, pos)) =>
    switch (pos_of(from_k), pos_of(to_k)) {
    | (Some(fp), Some(tp)) =>
      Some((
        from_k,
        to_k,
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
              y: apex.y -. 6.,
            },
            label_anchor: {
              x: apex.x,
              y: apex.y -. 6.,
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
            label_anchor: {
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
            label_anchor: {
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

  /* ---- label placement: multi-direction search avoiding BOTH other
     labels and node circles. A label chip renders translate(-50%,-100%):
     its visual box spans x ± w/2 and y-18..y. Candidates spiral outward
     from the natural anchor so dense clusters spread around their
     neighborhood instead of stacking into a column over the nodes. ---- */
  let edge_layouts = {
    let label_half = (el: edge_layout): float =>
      max(24., float_of_int(String.length(el.edge.e_name)) *. 3.7 +. 12.);
    let placed_labels: ref(list((pos, float))) = ref([]);
    let label_hits_label = (p: pos, w: float): bool =>
      List.exists(
        ((q, qw)) =>
          abs_float(q.x -. p.x) < w +. qw && abs_float(q.y -. p.y) < 20.,
        placed_labels^,
      );
    let label_hits_node = (p: pos, w: float): bool =>
      List.exists(
        (nl: node_layout) => {
          let cx = min(max(nl.p.x, p.x -. w), p.x +. w)
          and cy = min(max(nl.p.y, p.y -. 18.), p.y +. 2.);
          Float.hypot(nl.p.x -. cx, nl.p.y -. cy) < nl.r +. 4.;
        },
        node_layouts,
      );
    /* orbit rings are obstacles too: a chip sitting ON the ring line
       reads as part of the circle */
    let orbit_rings =
      List.filter_map(
        (el: edge_layout) =>
          el.endo
            ? Some((
                el.dst_p,
                radius_of(el.edge.dst)
                +. 16.
                +. float_of_int(el.orbit_rank)
                *. 15.,
              ))
            : None,
        edge_layouts,
      );
    let label_hits_ring = (p: pos, w: float): bool =>
      List.exists(
        ((c: pos, ring_r: float)) => {
          let cx = min(max(c.x, p.x -. w), p.x +. w)
          and cy = min(max(c.y, p.y -. 18.), p.y +. 2.);
          let d = Float.hypot(c.x -. cx, c.y -. cy);
          /* box straddles or touches the ring band */
          abs_float(d -. ring_r) < 12. || d < ring_r && ring_r -. d < 26.;
        },
        orbit_rings,
      );
    let candidates = [
      (0., 0.),
      (0., (-24.)),
      (0., 22.),
      (52., 0.),
      ((-52.), 0.),
      (52., (-24.)),
      ((-52.), (-24.)),
      (52., 22.),
      ((-52.), 22.),
      (0., (-48.)),
      (0., 44.),
      (104., 0.),
      ((-104.), 0.),
      (0., (-72.)),
      (0., 66.),
      (52., 44.),
      ((-52.), 44.),
      (104., (-22.)),
      ((-104.), (-22.)),
      (104., 22.),
      ((-104.), 22.),
      (52., (-44.)),
      ((-52.), (-44.)),
      (0., 88.),
      (156., 0.),
      ((-156.), 0.),
    ];
    /* Endo-family labels (orbits + feedback loops on one hub) stack in
       an aligned column directly above their hub — a legend attached to
       the cluster — instead of scattering around cramped arc geometry.
       (Learned from andrew's manual arrangements.) Ordinary edges keep
       on-edge labels, dodging via the candidate search. */
    let is_loop_edge = (el: edge_layout): bool =>
      List.mem((el.edge.e_src, el.edge.dst), loop_product_edges);
    /* legend membership is GEOMETRIC: a loop label stacks only while
       its arc is too short to carry it — stretch the edge (drag the
       product away) and the label comes back to live on it. Orbits
       always stack (their edge is a circle). */
    let edge_len = (el: edge_layout): float =>
      Float.hypot(el.dst_p.x -. el.src_p.x, el.dst_p.y -. el.src_p.y);
    let hub_of = (el: edge_layout): option(string) =>
      el.endo
      || is_loop_edge(el)
      && edge_len(el) < label_half(el)
      *. 2.
      +. 32.
        ? Some(el.edge.dst) : None;
    let hubs =
      List.filter_map(hub_of, edge_layouts) |> List.sort_uniq(compare);
    let stacked: Hashtbl.t(string, pos) = Hashtbl.create(8);
    List.iter(
      hub => {
        let members =
          edge_layouts
          |> List.filter(el => hub_of(el) == Some(hub))
          /* orbit labels first, then loops by their product height */
          /* bottom-up: loop with the closest product first, orbit label
             topmost (matches how the arcs nest visually) */
          |> List.stable_sort((a: edge_layout, b: edge_layout) =>
               switch (a.endo, b.endo) {
               | (true, false) => 1
               | (false, true) => (-1)
               | _ => compare(b.src_p.y, a.src_p.y)
               }
             );
        switch (pos_of(hub)) {
        | None => ()
        | Some(hp) =>
          /* legend base: clear of orbit rings, and of loop products only
             when they actually sit in the column above the hub (they
             prefer up-LEFT precisely to keep this column free) */
          let ring_top =
            List.fold_left(
              (acc, el: edge_layout) =>
                el.endo && el.edge.dst == hub
                  ? min(
                      acc,
                      hp.y
                      -. (
                        radius_of(hub)
                        +. 16.
                        +. float_of_int(el.orbit_rank)
                        *. 15.
                      ),
                    )
                  : el.edge.dst == hub
                    && is_loop_edge(el)
                    && abs_float(el.src_p.x -. hp.x) < 56.
                      ? min(acc, el.src_p.y -. 14.) : acc,
              hp.y -. radius_of(hub),
              edge_layouts,
            );
          List.iteri(
            (i, el: edge_layout) => {
              let base =
                snap(ring_top -. 14. -. grid /. 2. -. 9.) +. grid /. 2. +. 9.;
              let p = {
                x: snap(hp.x),
                y: base -. float_of_int(i) *. 28.,
              };
              Hashtbl.replace(stacked, el.edge.e_name, p);
              placed_labels := [(p, label_half(el)), ...placed_labels^];
            },
            members,
          );
        };
      },
      hubs,
    );
    List.map(
      (el: edge_layout) => {
        switch (Hashtbl.find_opt(stacked, el.edge.e_name)) {
        | Some(p) => {
            ...el,
            label_p: p,
            /* the stack is contextually attached: no leader line */
            label_anchor: p,
          }
        | None =>
          let w = label_half(el);
          let ok = (p: pos): bool =>
            !label_hits_label(p, w)
            && !label_hits_node(p, w)
            && !label_hits_ring(p, w);
          /* labels prefer LIVING ON THEIR EDGE: before jumping off,
             slide along the curve to nearby parameters */
          let curve_at = (t: float): pos => {
            let u = 1. -. t;
            let b = (a, b, c, d) =>
              u
              *. u
              *. u
              *. a
              +. 3.
              *. u
              *. u
              *. t
              *. b
              +. 3.
              *. u
              *. t
              *. t
              *. c
              +. t
              *. t
              *. t
              *. d;
            {
              x: b(el.src_p.x, el.c1.x, el.c2.x, el.dst_p.x),
              y: b(el.src_p.y, el.c1.y, el.c2.y, el.dst_p.y) -. 6.,
            };
          };
          let snap_label = (p: pos): pos => {
            /* chip renders translate(-50%,-100%): its text center sits
               ~10px above label_p. Dot centers are at grid multiples
               (nodes snap there); putting the text center halfway
               between dot rows means label_p.y = k*grid + grid/2 + 10 */
            x: snap(p.x),
            y: snap(p.y -. grid /. 2. -. 9.) +. grid /. 2. +. 9.,
          };
          let rec pick_abs = (ps: list(pos), fallback) =>
            switch (ps) {
            | [] => fallback()
            | [p0, ...rest] =>
              let p = snap_label(p0);
              ok(p) ? p : pick_abs(rest, fallback);
            };
          let rec pick = (cs: list((float, float))): pos =>
            switch (cs) {
            | [] => snap_label(el.label_p)
            | [(dx, dy), ...rest] =>
              let p =
                snap_label({
                  x: el.label_p.x +. dx,
                  y: el.label_p.y +. dy,
                });
              ok(p) ? p : pick(rest);
            };
          let on_curve =
            el.endo
              ? []
              : List.map(curve_at, [0.5, 0.42, 0.58, 0.34, 0.66, 0.26, 0.74]);
          let p = pick_abs(on_curve, () => pick(candidates));
          placed_labels := [(p, w), ...placed_labels^];
          {
            ...el,
            label_p: p,
          };
        }
      },
      edge_layouts,
    );
  };

  /* ---- values orbit their type's node: anchor = host center, vr
     staggers rings when a type has several constants ---- */
  let value_layouts = {
    let seen: Hashtbl.t(string, int) = Hashtbl.create(8);
    List.map(
      (v: CanvasGraph.value) => {
        let anchor = Option.value(~default=fallback, pos_of(v.v_key));
        let idx =
          switch (Hashtbl.find_opt(seen, v.v_key)) {
          | Some(n) =>
            Hashtbl.replace(seen, v.v_key, n + 1);
            n + 1;
          | None =>
            Hashtbl.replace(seen, v.v_key, 0);
            0;
          };
        {
          value: v,
          p: anchor,
          vr: radius_of(v.v_key) +. 13. +. float_of_int(idx) *. 7.,
        };
      },
      g.values,
    );
  };

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
            min(x0, el.label_p.x -. 62.),
            min(y0, el.label_p.y -. 18.),
            max(x1, el.label_p.x +. 62.),
            max(y1, el.label_p.y +. 4.),
          ),
        b,
        edge_layouts,
      );
    List.fold_left(
      ((x0, y0, x1, y1), vl: value_layout) =>
        (
          min(x0, vl.p.x -. vl.vr),
          min(y0, vl.p.y -. vl.vr),
          max(x1, vl.p.x +. vl.vr),
          max(y1, vl.p.y +. vl.vr),
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
    /* the normalization shift is grid-aligned so snapped coordinates
       stay on the dot lattice; when the pane is wider than the content,
       center within it (also on-grid) instead of hugging the left */
    let content_w = max_x -. min_x +. 2. *. pad;
    let center_pad =
      switch (center_within) {
      | Some(cw) when cw > content_w => snap((cw -. content_w) /. 2.)
      | _ => 0.
      };
    let (dx, dy) =
      switch (origin_override) {
      | Some(o) => (o.x, o.y)
      | None => (snap(pad -. min_x) +. center_pad, snap(pad -. min_y))
      };
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
              label_anchor: sh(el.label_anchor),
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
      formations:
        List.map(((ka, kb, a, b)) => (ka, kb, sh(a), sh(b)), formations),
      dep_links:
        List.map(((ka, kb, a, b)) => (ka, kb, sh(a), sh(b)), dep_links),
      /* panned-up content can put the extent above the origin; svg
         size attrs reject negatives (overlays overflow: visible) */
      width: max(0., max_x +. dx +. pad),
      height: max(0., max_y +. dy +. pad),
      origin: {
        x: dx,
        y: dy,
      },
    };
  };
};
