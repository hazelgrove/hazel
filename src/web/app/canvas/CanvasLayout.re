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
  | Product => 7.
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

/* How a non-grid node docks to its anchor. */
type dock =
  | DockIn /* builtin terminal on the input side */
  | DockOut /* builtin terminal on the output side */
  | DockLoop; /* loop product: above-left of its result component */

let layout = (g: CanvasGraph.t): t => {
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
        | None => None
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
                  x: margin +. float_of_int(l) *. col_w +. col_w /. 2.,
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
        let p =
          switch (d) {
          | DockIn => {
              x: a.x -. ar -. 92.,
              y: a.y -. 20. +. fi *. 42.,
            }
          | DockOut => {
              x: a.x +. ar +. 92.,
              y: a.y -. 20. +. fi *. 42.,
            }
          | DockLoop => {
              x: a.x -. ar -. 72. -. fi *. 24.,
              y: a.y -. ar -. 38. -. fi *. 34.,
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
  let node_layouts = grid_layouts @ docked_layouts^;
  let pos_of = (k: string): option(pos) =>
    Hashtbl.find_opt(placed, k) |> Option.map(fst);
  let radius_of = (k: string): float =>
    Hashtbl.find_opt(placed, k)
    |> Option.map(snd)
    |> Option.value(~default=base_radius);

  /* ---- formation + dependency links ---- */
  let formations =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        switch (n.kind, pos_of(n.key)) {
        | (Product, Some(pp)) =>
          n.parts |> List.filter_map(pos_of) |> List.map(cp => (cp, pp))
        | _ => []
        },
      g.nodes,
    );
  let dep_links =
    List.concat_map(
      (n: CanvasGraph.tynode) =>
        switch (n.kind, pos_of(n.key)) {
        | (Alias, Some(np)) =>
          n.deps
          |> List.filter(d => d != n.key)
          |> List.filter_map(pos_of)
          |> List.map(dp => (dp, np))
        | _ => []
        },
      g.nodes,
    );

  /* ---- edges ---- */
  let fallback = {
    x: margin,
    y: margin,
  };
  let orbit_seen: Hashtbl.t(string, int) = Hashtbl.create(4);
  let edge_layouts =
    List.map(
      (e: CanvasGraph.edge) => {
        let src_p = Option.value(~default=fallback, pos_of(e.e_src));
        let dst_p = Option.value(~default=fallback, pos_of(e.dst));
        let endo = e.e_src == e.dst;
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

  /* ---- bounds from actual positions (docked nodes overflow the grid) ---- */
  let (width, height) =
    List.fold_left(
      ((w, h), nl: node_layout) =>
        (max(w, nl.p.x +. nl.r +. 90.), max(h, nl.p.y +. nl.r +. 60.)),
      (
        float_of_int(max_layer + 1) *. col_w +. 2. *. margin,
        float_of_int(max_rows) *. row_h +. 2. *. margin,
      ),
      node_layouts,
    );

  {
    nodes: node_layouts,
    edges: edge_layouts,
    values: value_layouts,
    formations,
    dep_links,
    width,
    height,
  };
};
