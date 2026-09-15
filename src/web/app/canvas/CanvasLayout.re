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
  /* the chip sits ON the wire (centered on the curve, masking it) rather
     than above it: only when the arrow is long enough to show its head
     and a stretch of line on both sides */
  on_wire: bool,
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
/* ---- routing for links and arrows ----
   Lines are STRAIGHT. A chain laid in one row (`Int -> Pos -> World`) would
   put the middle node on the long link, so `declutter` below moves such a
   node off the chord at layout time; while a node is being dragged a line
   may pass under it until the release re-lays the graph. (The earlier
   router bent a link around whatever it crossed, and re-bent it on every
   drag frame as the crossings changed: the "waves".) Pure, so the drag
   follower can re-route from positions alone. */
let route_link =
    (
      ~nodes as _: list(node_layout),
      ~from_key as _: string,
      ~to_key as _: string,
      a: pos,
      b: pos,
    )
    : (pos, pos) => {
  let c = (t: float) => {
    x: a.x +. (b.x -. a.x) *. t,
    y: a.y +. (b.y -. a.y) *. t,
  };
  (c(0.33), c(0.67));
};

let link_d = (a: pos, c1: pos, c2: pos, b: pos): string =>
  Printf.sprintf(
    "M %.1f,%.1f C %.1f,%.1f %.1f,%.1f %.1f,%.1f",
    a.x,
    a.y,
    c1.x,
    c1.y,
    c2.x,
    c2.y,
    b.x,
    b.y,
  );

/* ---- declutter: nodes step OFF the straight lines between other nodes.
   For every link/arrow chord, a node the chord would run through (not its
   ends) is nudged perpendicular to the chord by the clearance it lacks;
   exactly collinear nodes alternate sides. A few rounds settle chains.
   Fixed nodes (pinned or dragged by the user) stay; a docked satellite
   rides its anchor. */
let declutter =
    (
      ~chords: list((string, string)),
      ~fixed: list(string),
      ~radius_of: string => float,
      nodes: list(node_layout),
    )
    : list(node_layout) => {
  let clear = 12.;
  let tbl: Hashtbl.t(string, pos) = Hashtbl.create(32);
  List.iter(
    (nl: node_layout) => Hashtbl.replace(tbl, nl.node.key, nl.p),
    nodes,
  );
  let anchor_of = (nl: node_layout): option(string) =>
    switch (nl.node.sat) {
    | Some((a, _)) => Some(a)
    | None => None
    };
  let movable = (nl: node_layout): bool =>
    !List.mem(nl.node.key, fixed) && anchor_of(nl) == None;
  let parity: Hashtbl.t(string, float) = Hashtbl.create(32);
  List.iteri(
    (i, nl: node_layout) =>
      Hashtbl.replace(parity, nl.node.key, i mod 2 == 0 ? 1. : (-1.)),
    nodes,
  );
  for (_ in 1 to 6) {
    let delta: Hashtbl.t(string, (float, float)) = Hashtbl.create(16);
    List.iter(
      ((ka, kb)) =>
        switch (Hashtbl.find_opt(tbl, ka), Hashtbl.find_opt(tbl, kb)) {
        | (Some(a), Some(b)) =>
          let dx = b.x -. a.x
          and dy = b.y -. a.y;
          let len = sqrt(dx *. dx +. dy *. dy);
          if (len > 1.) {
            let (ux, uy) = (dx /. len, dy /. len);
            let (nx, ny) = (-. uy, ux);
            List.iter(
              (nl: node_layout) =>
                if (nl.node.key != ka && nl.node.key != kb && movable(nl)) {
                  let p = Hashtbl.find(tbl, nl.node.key);
                  let r = radius_of(nl.node.key);
                  let px = p.x -. a.x
                  and py = p.y -. a.y;
                  let along = px *. ux +. py *. uy;
                  let side = px *. nx +. py *. ny;
                  let need = r +. clear;
                  if (along > r && along < len -. r && abs_float(side) < need) {
                    let dir =
                      abs_float(side) < 0.5
                        ? Hashtbl.find(parity, nl.node.key)
                        : side >= 0. ? 1. : (-1.);
                    let m = need -. abs_float(side);
                    let (ox, oy) =
                      Option.value(
                        ~default=(0., 0.),
                        Hashtbl.find_opt(delta, nl.node.key),
                      );
                    Hashtbl.replace(
                      delta,
                      nl.node.key,
                      (ox +. dir *. nx *. m, oy +. dir *. ny *. m),
                    );
                  };
                },
              nodes,
            );
          };
        | _ => ()
        },
      chords,
    );
    Hashtbl.iter(
      (k, (ox, oy)) => {
        let p = Hashtbl.find(tbl, k);
        Hashtbl.replace(
          tbl,
          k,
          {
            x: p.x +. ox,
            y: p.y +. oy,
          },
        );
      },
      delta,
    );
    List.iter(
      (nl: node_layout) =>
        switch (anchor_of(nl)) {
        | Some(a) =>
          switch (Hashtbl.find_opt(delta, a)) {
          | Some((ox, oy)) =>
            let p = Hashtbl.find(tbl, nl.node.key);
            Hashtbl.replace(
              tbl,
              nl.node.key,
              {
                x: p.x +. ox,
                y: p.y +. oy,
              },
            );
          | None => ()
          }
        | None => ()
        },
      nodes,
    );
  };
  List.map(
    (nl: node_layout) =>
      {
        ...nl,
        p: Hashtbl.find(tbl, nl.node.key),
      },
    nodes,
  );
};

/* A docked terminal can lie directly on its anchor's other relationship.
   Move that terminal off the shared chord before applying manual positions;
   the user's later drag remains rigid and never relocates bystanders. */
let separate_docked_chords =
    (~chords: list((string, string)), nodes: list(node_layout)) => {
  let positions = Hashtbl.create(32);
  List.iter(
    (nl: node_layout) => Hashtbl.replace(positions, nl.node.key, nl.p),
    nodes,
  );
  List.map(
    (nl: node_layout) => {
      let p =
        List.fold_left(
          (p: pos, (ka, kb)) =>
            switch (
              nl.node.kind,
              nl.node.sat,
              Hashtbl.find_opt(positions, ka),
              Hashtbl.find_opt(positions, kb),
            ) {
            | (Builtin, Some((anchor, _)), Some(a), Some(b))
                when
                  nl.node.key != ka
                  && nl.node.key != kb
                  && (anchor == ka || anchor == kb) =>
              let length = sqrt((b.x -. a.x) ** 2. +. (b.y -. a.y) ** 2.);
              let u = norm(a, b);
              let along = (p.x -. a.x) *. u.x +. (p.y -. a.y) *. u.y;
              let side = (p.y -. a.y) *. u.x -. (p.x -. a.x) *. u.y;
              if (length > 1.
                  && along > 0.
                  && along < length
                  && abs_float(side) < nl.r
                  +. 12.) {
                let candidate = dir => {
                  x: p.x -. u.y *. dir *. 42.,
                  y: p.y +. u.x *. dir *. 42.,
                };
                let clearance = (q: pos) =>
                  List.fold_left(
                    (score, other: node_layout) =>
                      other.node.key == nl.node.key
                        ? score
                        : min(
                            score,
                            sqrt(
                              (q.x -. other.p.x)
                              ** 2.
                              +. (q.y -. other.p.y)
                              ** 2.,
                            )
                            -. other.r,
                          ),
                    max_float,
                    nodes,
                  );
                let above = candidate(-1.)
                and below = candidate(1.);
                clearance(above) >= clearance(below) ? above : below;
              } else {
                p;
              };
            | _ => p
            },
          nl.p,
          chords,
        );
      {
        ...nl,
        p,
      };
    },
    nodes,
  );
};

/* Manual movement is applied after automatic layout. Docked satellites
   inherit their anchor's movement; an explicit satellite pin stays put. */
let apply_manual_positions = (~offsets, ~pins, nodes: list(node_layout)) => {
  let rec displacement = (seen, nl: node_layout): (float, float) =>
    if (List.mem(nl.node.key, seen)) {
      (0., 0.);
    } else {
      switch (List.assoc_opt(nl.node.key, pins)) {
      | Some((x, y)) => (snap(x) -. nl.p.x, snap(y) -. nl.p.y)
      | None =>
        let (dx, dy) =
          Option.value(
            ~default=(0., 0.),
            List.assoc_opt(nl.node.key, offsets),
          );
        let (ax, ay) =
          switch (nl.node.sat) {
          | Some((anchor, _)) =>
            List.find_opt((n: node_layout) => n.node.key == anchor, nodes)
            |> Option.map(n => displacement([nl.node.key, ...seen], n))
            |> Option.value(~default=(0., 0.))
          | None => (0., 0.)
          };
        (dx +. ax, dy +. ay);
      };
    };
  List.map(
    (nl: node_layout) => {
      let (dx, dy) = displacement([], nl);
      {
        ...nl,
        p: {
          x: nl.p.x +. dx,
          y: nl.p.y +. dy,
        },
      };
    },
    nodes,
  );
};

/* Comparison modes keep the same graph and routing; only the rank
   constraints change. Combined remains the production default. */
type ranking =
  | Combined
  | FunctionFlow
  | TypeDependencies;
let layout_impl =
    (
      ~ranking=Combined,
      ~x_scale=1.,
      ~y_scale=1.,
      ~center_within: option(float)=None,
      /* fix the normalization origin (from a no-offsets layout of the
         same graph): user drags must never re-anchor the frame, else
         dragging feeds back into a whole-graph shift */
      ~origin_override: option(pos)=None,
      ~offsets: list((string, (float, float)))=[],
      ~pins: list((string, (float, float)))=[],
      /* expanded type cards: (w, h) per node key. A card is DISPLAY
         only: edges land on its rectangle and the board grows to hold
         it, but it never moves a node — toggling a card is not a
         relayout (the whole-graph re-solve and re-frame it caused threw
         the view in a small pane; neighbours may overlap for now). */
      ~cards: list((string, (float, float)))=[],
      g: CanvasGraph.t,
    )
    : t => {
  let t_pre = Util.PerfTimer.now();
  let card_of = (k: string): option((float, float)) =>
    List.assoc_opt(k, cards);
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
        /* folded former: the alias's own components rank before it */
        | (Alias, _) when n.parts != [] =>
          List.map(
            pk =>
              Util.GraphLayout.Spec.{
                src: pk,
                dst: n.key,
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
        || n.kind == CanvasGraph.Alias
        && n.parts != []
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
  /* member functions pull their module former toward the types they
     touch: unranked ordering edges (row attraction, no column rank) */
  let module_flow_pull =
    List.concat_map(
      (e: CanvasGraph.edge) =>
        switch (e.m_path) {
        | [root, ..._] =>
          let fk = "{}@" ++ root;
          List.mem(fk, grid_keys)
            ? List.filter_map(
                ((k, ranked)) =>
                  k != fk && List.mem(k, grid_keys)
                    ? Some(
                        Util.GraphLayout.Spec.{
                          src: k,
                          dst: fk,
                          ranked,
                        },
                      )
                    : None,
                /* rank the former after member INPUT types so it lands
                   a column over from them; results only order rows */
                [(e.e_src, true), (e.dst, false)],
              )
            : [];
        | [] => []
        },
      g.edges,
    );
  /* pinned nodes with no spec relationships float FREE of the rank
     grid: a freshly placed node would otherwise claim a row and shove
     every auto-laid node down */
  let unrank =
    List.map((e: Util.GraphLayout.Spec.edge) =>
      {
        ...e,
        ranked: false,
      }
    );
  let type_edges = es => ranking == FunctionFlow ? unrank(es) : es;
  let all_spec_edges =
    type_edges(dep_edges @ derived_edges)
    @ (ranking == TypeDependencies ? unrank(flow_edges) : flow_edges)
    @ type_edges(former_rank_edges)
    @ formation_edges
    @ type_edges(module_flow_pull);
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
  Util.PerfTimer.record("layout/pre", Util.PerfTimer.now() -. t_pre);
  let t_solve = Util.PerfTimer.now();
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
      edges: all_spec_edges,
      attachments,
      col_gap: 160.,
      row_gap: 96.,
      margin,
      x_stretch: x_scale,
      y_stretch: y_scale,
      order_sweeps: 4,
      order_hysteresis: 0.6,
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

  /* Declutter the automatic arrangement once, before manual placement.
     A pointer movement must not push unrelated nodes out of its new
     chords. Routing adapts to the final positions below instead. */
  Util.PerfTimer.record("layout/solve", Util.PerfTimer.now() -. t_solve);
  let t_nodes = Util.PerfTimer.now();
  let node_layouts =
    List.map(
      (nl: node_layout) =>
        {
          ...nl,
          p: {
            x: snap(nl.p.x),
            y: snap(nl.p.y),
          },
        },
      placed_layouts,
    );
  /* every straight line the render will draw: function arrows and the
     "made of" links (parts → product, former → alias, element → [T],
     alias-body deps → alias) */
  let node_layouts = {
    let chords =
      List.map((e: CanvasGraph.edge) => (e.e_src, e.dst), g.edges)
      @ List.concat_map(
          (n: CanvasGraph.tynode) =>
            switch (n.kind) {
            | Product =>
              List.map(pk => (pk, n.key), n.parts)
              @ (
                switch (n.sat) {
                | Some((anchor, _)) => [(n.key, anchor)]
                | None => []
                }
              )
            | Alias =>
              List.map(pk => (pk, n.key), n.parts)
              @ List.filter_map(
                  d =>
                    d != n.key && !List.mem(d, n.hidden_deps)
                      ? Some((d, n.key)) : None,
                  n.deps,
                )
            | Derived =>
              switch (strip_brackets(n.key)) {
              | Some(ik) => [(ik, n.key)]
              | None => []
              }
            | _ => []
            },
          g.nodes,
        );
    let circle_r = (k: string): float =>
      List.find_opt((nl: node_layout) => nl.node.key == k, node_layouts)
      |> Option.map((nl: node_layout) => nl.r)
      |> Option.value(~default=base_radius);
    declutter(~chords, ~fixed=free_keys, ~radius_of=circle_r, node_layouts)
    |> separate_docked_chords(~chords);
  };
  let node_layouts = apply_manual_positions(~offsets, ~pins, node_layouts);
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
  /* edge endpoints land on the node's rim: the circle, or for a card
     the point where the ray toward `b` leaves its rectangle */
  let rim_toward = (a: pos, b: pos, k: string): pos =>
    switch (card_of(k)) {
    | None => offset_along(a, b, radius_of(k))
    | Some((w, h)) =>
      let u = norm(a, b);
      let tx =
        Float.abs(u.x) < 1e-6 ? Float.infinity : w /. 2. /. Float.abs(u.x);
      let ty =
        Float.abs(u.y) < 1e-6 ? Float.infinity : h /. 2. /. Float.abs(u.y);
      let t = Float.min(tx, ty) +. 1.;
      {
        x: a.x +. u.x *. t,
        y: a.y +. u.y *. t,
      };
    };
  let half_w_of = (k: string): float =>
    switch (card_of(k)) {
    | Some((w, _)) => w /. 2. +. 1.
    | None => radius_of(k)
    };

  /* ---- formation + dependency links (rim-to-rim so arrowheads land) ---- */
  let rim_pair =
      (from_k: string, to_k: string): option((string, string, pos, pos)) =>
    switch (pos_of(from_k), pos_of(to_k)) {
    | (Some(fp), Some(tp)) =>
      Some((
        from_k,
        to_k,
        rim_toward(fp, tp, from_k),
        rim_toward(tp, fp, to_k),
      ))
    | _ => None
    };
  let formations =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        switch (n.kind) {
        | Product =>
          (
            List.sort_uniq(compare, n.parts)
            |> List.filter_map(pk => rim_pair(pk, n.key))
          )
          /* alias-body formers dock at their alias and feed it */
          @ (
            switch (n.sat) {
            | Some((anchor, _)) => rim_pair(n.key, anchor) |> Option.to_list
            | None => []
            }
          )
        /* folded former: components feed the alias node directly */
        | Alias when n.parts != [] =>
          List.sort_uniq(compare, n.parts)
          |> List.filter_map(pk => rim_pair(pk, n.key))
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
  Util.PerfTimer.record("layout/nodes", Util.PerfTimer.now() -. t_nodes);
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
            on_wire: false,
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
          /* arrows run rim to rim: the head lands on the codomain node */
          let s = rim_toward(src_p, ctrl, e.e_src);
          let d = rim_toward(dst_p, ctrl, e.dst);
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
            on_wire: false,
            label_anchor: {
              x: ctrl.x +. v.x *. 14.,
              y: ctrl.y +. v.y *. 14. -. 4.,
            },
          };
        } else {
          let sign = dst_p.x >= src_p.x ? 1. : (-1.);
          let s = {
            x: src_p.x +. sign *. half_w_of(e.e_src),
            y: src_p.y,
          };
          let d = {
            x: dst_p.x -. sign *. half_w_of(e.dst),
            y: dst_p.y,
          };
          let bend = max(24., min(90., abs_float(d.x -. s.x) *. 0.5));
          /* straight: an S with horizontal handles (declutter has moved
             the nodes off the chord); a same-column arrow runs down the
             column */
          let same_col = abs_float(d.x -. s.x) < 60.;
          let c1 = {
            x: same_col ? s.x : s.x +. sign *. bend,
            y: s.y,
          };
          let c2 = {
            x: same_col ? d.x : d.x -. sign *. bend,
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
            on_wire: false,
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
            on_wire: false,
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
          /* ON THE WIRE when the arrow is long enough: the chip (2w wide)
             must leave the arrowhead (9) plus a stretch of line (14)
             visible on both sides; the chip is centered on the curve, so
             the hit boxes (built for a chip standing above its point) are
             checked with the point moved to where such a chip would sit */
          let chord =
            Float.hypot(el.dst_p.x -. el.src_p.x, el.dst_p.y -. el.src_p.y);
          let fits_on_wire = !el.endo && chord -. 2. *. w >= 2. *. (9. +. 14.);
          let on_wire_p =
            if (fits_on_wire) {
              List.map(
                t => {
                  let c = curve_at(t);
                  {
                    x: c.x,
                    y: c.y +. 6.,
                  };
                },
                [0.5, 0.42, 0.58, 0.34, 0.66, 0.28, 0.72],
              )
              |> List.find_opt((c: pos) =>
                   ok({
                     x: c.x,
                     y: c.y +. 9.,
                   })
                 );
            } else {
              None;
            };
          switch (on_wire_p) {
          | Some(p) =>
            placed_labels :=
              [
                (
                  {
                    x: p.x,
                    y: p.y +. 9.,
                  },
                  w,
                ),
                ...placed_labels^,
              ];
            {
              ...el,
              label_p: p,
              label_anchor: p,
              on_wire: true,
            };
          | None =>
            let on_curve =
              el.endo
                ? []
                : List.map(
                    curve_at,
                    [0.5, 0.42, 0.58, 0.34, 0.66, 0.26, 0.74],
                  );
            let p = pick_abs(on_curve, () => pick(candidates));
            placed_labels := [(p, w), ...placed_labels^];
            {
              ...el,
              label_p: p,
            };
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
         size attrs reject negatives (overlays overflow: visible). The
         board also reaches past any card's right/bottom edge (cards do
         not enter the extent that positions the graph, so a card can't
         shift it — but it must be scrollable to). */
      width:
        List.fold_left(
          (acc, (k, (w, _))) =>
            switch (
              List.find_opt(
                (nl: node_layout) => nl.node.key == k,
                node_layouts,
              )
            ) {
            | Some(nl) => max(acc, nl.p.x +. dx +. w /. 2. +. pad)
            | None => acc
            },
          max(0., max_x +. dx +. pad),
          cards,
        ),
      height:
        List.fold_left(
          (acc, (k, (_, h))) =>
            switch (
              List.find_opt(
                (nl: node_layout) => nl.node.key == k,
                node_layouts,
              )
            ) {
            | Some(nl) => max(acc, nl.p.y +. dy +. h /. 2. +. pad)
            | None => acc
            },
          max(0., max_y +. dy +. pad),
          cards,
        ),
      origin: {
        x: dx,
        y: dy,
      },
    };
  };
};

/* ---- instrumentation: how often and how long a render lays out ---- */
let layout_calls: ref(int) = ref(0);
let layout_ms: ref(float) = ref(0.);
/* Memo: a render lays the same graph out several times (fit check, frame
   viability, the view), and a score's renders repeat with nothing changed —
   ~100 layouts per tool call at 120 nodes (17 ms each) before this. Keyed
   on the graph's identity (extract is memoized per statics, so it is
   stable) and the parameters by value; the last few results are kept. */
type layout_key = {
  k_graph: CanvasGraph.t,
  k_x: float,
  k_y: float,
  k_center: option(float),
  k_origin: option(pos),
  k_offsets: list((string, (float, float))),
  k_pins: list((string, (float, float))),
  k_cards: list((string, (float, float))),
};
let layout_memo: ref(list((layout_key, t))) = ref([]);
let layout_memo_hits: ref(int) = ref(0);
/* the expanded cards' extents for the layouts of the current render:
   set by CanvasSidebar before laying out (every layout call of a render
   sees the same cards, so this is not threaded through each call) */
let card_extents: ref(list((string, (float, float)))) = ref([]);
let layout =
    (
      ~x_scale=1.,
      ~y_scale=1.,
      ~center_within: option(float)=None,
      ~origin_override: option(pos)=None,
      ~offsets: list((string, (float, float)))=[],
      ~pins: list((string, (float, float)))=[],
      ~cards: option(list((string, (float, float))))=?,
      g: CanvasGraph.t,
    )
    : t => {
  let cards = Option.value(~default=card_extents^, cards);
  let key = {
    k_graph: g,
    k_x: x_scale,
    k_y: y_scale,
    k_center: center_within,
    k_origin: origin_override,
    k_offsets: offsets,
    k_pins: pins,
    k_cards: cards,
  };
  let same = (k: layout_key) =>
    k.k_graph === g
    && k.k_x == x_scale
    && k.k_y == y_scale
    && k.k_center == center_within
    && k.k_origin == origin_override
    && k.k_offsets == offsets
    && k.k_pins == pins
    && k.k_cards == cards;
  switch (List.find_opt(((k, _)) => same(k), layout_memo^)) {
  | Some((_, r)) =>
    incr(layout_memo_hits);
    r;
  | None =>
    let now = () =>
      Js_of_ocaml.Js.Unsafe.coerce(Js_of_ocaml.Js.Unsafe.global)##._Date##now();
    let t0 = now();
    let r =
      layout_impl(
        ~x_scale,
        ~y_scale,
        ~center_within,
        ~origin_override,
        ~offsets,
        ~pins,
        ~cards,
        g,
      );
    incr(layout_calls);
    layout_ms := layout_ms^ +. (now() -. t0);
    Util.PerfTimer.record("canvas-layout", now() -. t0);
    layout_memo :=
      [(key, r), ...List.filteri((i, _) => i < 5, layout_memo^)];
    r;
  };
};
