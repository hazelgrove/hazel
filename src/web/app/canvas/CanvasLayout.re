/* CanvasLayout — deterministic layered layout for CanvasGraph.
   Left-to-right layers by type-dependency (aliases depend on the types
   they mention; a function's result sits right of its arguments), slots
   within a layer in program order. Motion between layouts is smoothed by
   the id-keyed FLIP in Animation.re, so this recomputes from scratch —
   position inheritance can come later if churn shows. */

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
  src_ps: list(pos), /* one per (found) source node */
  jct: pos, /* junction where multi-arg inputs meet */
  dst_p: pos,
  endo: bool, /* dst is also a source: render as orbit */
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
  | Builtin => 12.
  | Ghost => 14.
  | Derived => 15.
  | Alias => base_radius +. ctr_bump +. fan_bump
  };
};

let strip_brackets = (k: string): option(string) =>
  if (String.length(k) >= 2 && k.[0] == '[') {
    Some(String.sub(k, 1, String.length(k) - 2));
  } else {
    None;
  };

let layout = (g: CanvasGraph.t): t => {
  let keys = List.map((n: CanvasGraph.tynode) => n.key, g.nodes);
  let find_node = (k: string): option(CanvasGraph.tynode) =>
    List.find_opt((n: CanvasGraph.tynode) => n.key == k, g.nodes);

  /* ---- layer assignment ---- */
  /* 1. longest-path over alias deps (restricted to known keys). */
  let tbl: Hashtbl.t(string, int) = Hashtbl.create(16);
  let rec layer_of = (~depth=0, k: string): int =>
    switch (Hashtbl.find_opt(tbl, k)) {
    | Some(l) => l
    | None when depth > 32 => 0
    | None =>
      Hashtbl.replace(tbl, k, 0); /* cycle guard (Rec types) */
      let l =
        switch (find_node(k)) {
        | None => 0
        | Some(n) =>
          let dep_layers =
            n.deps
            |> List.filter(d => d != k && List.mem(d, keys))
            |> List.map(layer_of(~depth=depth + 1));
          let inner =
            switch (strip_brackets(k)) {
            | Some(ik) when List.mem(ik, keys) && ik != k => [
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
  List.iter(k => ignore(layer_of(k)), keys);
  /* 2. flow constraint: a function's result sits right of its arguments. */
  for (_ in 1 to 3) {
    List.iter(
      (e: CanvasGraph.edge) =>
        if (!List.mem(e.dst, e.srcs)) {
          let src_max =
            e.srcs
            |> List.filter(k => List.mem(k, keys))
            |> List.map(k => layer_of(k))
            |> List.fold_left(max, 0);
          switch (Hashtbl.find_opt(tbl, e.dst)) {
          | Some(l) when l <= src_max =>
            Hashtbl.replace(tbl, e.dst, src_max + 1)
          | _ => ()
          };
        },
      g.edges,
    );
  };
  let layer = (k: string): int =>
    switch (Hashtbl.find_opt(tbl, k)) {
    | Some(l) => l
    | None => 0
    };

  /* ---- slots ---- */
  let max_layer = List.fold_left((m, k) => max(m, layer(k)), 0, keys);
  let by_layer =
    List.init(max_layer + 1, l =>
      List.filter((n: CanvasGraph.tynode) => layer(n.key) == l, g.nodes)
    );
  let max_rows =
    List.fold_left((m, ns) => max(m, List.length(ns)), 1, by_layer);
  let height = float_of_int(max_rows) *. row_h +. 2. *. margin;
  let fan = (k: string): int =>
    List.length(
      List.filter(
        (e: CanvasGraph.edge) => List.mem(k, e.srcs) || e.dst == k,
        g.edges,
      ),
    );
  let node_layouts: list(node_layout) =
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
  let width = float_of_int(max_layer + 1) *. col_w +. 2. *. margin;
  let pos_of = (k: string): option(pos) =>
    List.find_opt((nl: node_layout) => nl.node.key == k, node_layouts)
    |> Option.map((nl: node_layout) => nl.p);
  let radius_of = (k: string): float =>
    List.find_opt((nl: node_layout) => nl.node.key == k, node_layouts)
    |> Option.map((nl: node_layout) => nl.r)
    |> Option.value(~default=base_radius);

  /* ---- edges ---- */
  let orbit_seen: Hashtbl.t(string, int) = Hashtbl.create(4);
  let edge_layouts =
    List.map(
      (e: CanvasGraph.edge) => {
        let dst_p =
          Option.value(
            ~default={
              x: margin,
              y: margin,
            },
            pos_of(e.dst),
          );
        let endo = List.mem(e.dst, e.srcs);
        let orbit_rank =
          if (endo) {
            let k =
              Option.value(~default=0, Hashtbl.find_opt(orbit_seen, e.dst));
            Hashtbl.replace(orbit_seen, e.dst, k + 1);
            k;
          } else {
            0;
          };
        let src_ps =
          e.srcs
          |> List.filter(k => !(endo && k == e.dst))
          |> List.filter_map(pos_of);
        let centroid =
          switch (src_ps) {
          | [] => dst_p
          | ps =>
            let n = float_of_int(List.length(ps));
            {
              x: List.fold_left((a, p) => a +. p.x, 0., ps) /. n,
              y: List.fold_left((a, p) => a +. p.y, 0., ps) /. n,
            };
          };
        let orbit_r =
          radius_of(e.dst) +. 16. +. float_of_int(orbit_rank) *. 15.;
        let jct =
          endo
            /* orbit apex above the node; stacked orbits stack outward */
            ? {
              x: dst_p.x,
              y: dst_p.y -. orbit_r -. 10.,
            }
            : {
              x: centroid.x +. (dst_p.x -. centroid.x) *. 0.55,
              y: centroid.y +. (dst_p.y -. centroid.y) *. 0.55,
            };
        let label_p =
          endo
            ? {
              x: jct.x,
              y: jct.y -. 2.,
            }
            : {
              x: (jct.x +. dst_p.x) /. 2.,
              y: (jct.y +. dst_p.y) /. 2. -. 8.,
            };
        {
          edge: e,
          src_ps,
          jct,
          dst_p,
          endo,
          orbit_rank,
          label_p,
        };
      },
      g.edges,
    );

  /* ---- values dock below their type's node ---- */
  let value_layouts =
    List.mapi(
      (i, v: CanvasGraph.value) => {
        let anchor =
          Option.value(
            ~default={
              x: margin,
              y: margin,
            },
            pos_of(v.v_key),
          );
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

  {
    nodes: node_layouts,
    edges: edge_layouts,
    values: value_layouts,
    width,
    height,
  };
};
