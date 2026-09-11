/* GraphLayout — see GraphLayout.rei for the API contract.

   Pipeline: rank (longest-path over ranked edges, cycle-guarded, empty
   columns compacted) → order (barycenter sweeps within columns) →
   coords (radius-aware column x / stacked y, then neighbor-aligned y
   relaxation that preserves order and gaps) → attach (collision-aware
   ring search around hosts, chained hosts resolved in passes). */

type pos = {
  x: float,
  y: float,
};

module Spec = {
  type node = {
    id: string,
    radius: float,
    /* extra reserved vertical clearance (e.g. a label legend attached
       above the node): spacing and relief treat it as part of the halo */
    extent_above: float,
    extent_below: float,
  };

  type edge = {
    src: string,
    dst: string,
    ranked: bool,
  };

  type side =
    | In
    | Out
    | Above
    | AboveLeft /* diagonal: keeps the column straight above the host free */
    | Below;

  type attachment = {
    id: string,
    host: string,
    radius: float,
    prefer: side,
    dist: float,
  };

  type t = {
    nodes: list(node),
    edges: list(edge),
    attachments: list(attachment),
    col_gap: float,
    row_gap: float,
    margin: float,
    x_stretch: float,
    y_stretch: float,
    order_sweeps: int,
    /* within-rank ordering is damped: a node passes a neighbor only when
       its barycenter beats the neighbor's by more than this (in row
       positions). Ties and weak pulls keep the current order — which
       starts as program order — so a new edge moves the nodes it
       genuinely pulls, not whole branches. 0 = plain barycenter sort. */
    order_hysteresis: float,
  };

  let default: t = {
    nodes: [],
    edges: [],
    attachments: [],
    col_gap: 110.,
    row_gap: 56.,
    margin: 70.,
    x_stretch: 1.,
    y_stretch: 1.,
    order_sweeps: 4,
    order_hysteresis: 0.6,
  };
};

type result = {
  positions: list((string, pos)),
  ranks: list((string, int)),
};

let pos_of = (r: result, id: string): option(pos) =>
  List.assoc_opt(id, r.positions);

let layout = (spec: Spec.t): result => {
  open Spec;
  let t_gl = PerfTimer.now();
  let node_ids = List.map((n: node) => n.id, spec.nodes);
  let node_set: Hashtbl.t(string, unit) = Hashtbl.create(64);
  List.iter(id => Hashtbl.replace(node_set, id, ()), node_ids);
  let is_node = (id: string) => Hashtbl.mem(node_set, id);
  /* per-node lookups are asked thousands of times per layout (every
     relaxation pass, every column height): tables, not list scans */
  let radius_tbl: Hashtbl.t(string, float) = Hashtbl.create(64);
  List.iter(
    (n: node) => Hashtbl.replace(radius_tbl, n.id, n.radius),
    spec.nodes,
  );
  let radius = (id: string): float =>
    switch (Hashtbl.find_opt(radius_tbl, id)) {
    | Some(r) => r
    | None => 10.
    };

  /* ---- 1. rank: longest-path relaxation over ranked edges ----
     Iteration count bounds cycle blowup: a DAG converges within
     |nodes| passes; cyclic remainders just stop advancing. */
  let rank_tbl: Hashtbl.t(string, int) = Hashtbl.create(16);
  List.iter(id => Hashtbl.replace(rank_tbl, id, 0), node_ids);
  let ranked_edges =
    List.filter(
      (e: edge) =>
        e.ranked && e.src != e.dst && is_node(e.src) && is_node(e.dst),
      spec.edges,
    );
  let passes = min(List.length(node_ids) + 1, 32);
  for (_ in 1 to passes) {
    List.iter(
      (e: edge) => {
        let sr = Hashtbl.find(rank_tbl, e.src)
        and dr = Hashtbl.find(rank_tbl, e.dst);
        if (dr <= sr && sr < 64) {
          Hashtbl.replace(rank_tbl, e.dst, sr + 1);
        };
      },
      ranked_edges,
    );
  };
  /* Simply-connected pull: a node with rank freedom (no ranked
     in-edges) drifts to the column just left of its nearest neighbor,
     so satellites-of-one-thing travel WITH that thing instead of
     stranding at rank 0 (andrew's Msg rule). Host-resolution applies:
     a neighbor that is an attachment counts as its host. */
  let has_ranked_in = (id: string): bool =>
    List.exists((e: edge) => e.dst == id, ranked_edges);
  let rec resolve_node = (~depth=0, id: string): option(string) =>
    if (is_node(id)) {
      Some(id);
    } else if (depth > 3) {
      None;
    } else {
      switch (List.find_opt((a: attachment) => a.id == id, spec.attachments)) {
      | Some(a) => resolve_node(~depth=depth + 1, a.host)
      | None => None
      };
    };
  List.iter(
    id =>
      if (!has_ranked_in(id)) {
        let neighbor_ranks =
          spec.edges
          |> List.filter_map((e: edge) =>
               if (e.src == id) {
                 resolve_node(e.dst);
               } else if (e.dst == id) {
                 resolve_node(e.src);
               } else {
                 None;
               }
             )
          |> List.filter(m => m != id)
          |> List.map(Hashtbl.find(rank_tbl));
        switch (List.sort(compare, neighbor_ranks)) {
        | [r_min, ..._] => Hashtbl.replace(rank_tbl, id, max(0, r_min - 1))
        | [] => ()
        };
      },
    node_ids,
  );
  /* re-run relaxation so ranked successors shift right if needed */
  for (_ in 1 to passes) {
    List.iter(
      (e: edge) => {
        let sr = Hashtbl.find(rank_tbl, e.src)
        and dr = Hashtbl.find(rank_tbl, e.dst);
        if (dr <= sr && sr < 64) {
          Hashtbl.replace(rank_tbl, e.dst, sr + 1);
        };
      },
      ranked_edges,
    );
  };
  /* compact used ranks */
  let used =
    node_ids |> List.map(Hashtbl.find(rank_tbl)) |> List.sort_uniq(compare);
  let rank = (id: string): int => {
    let raw = Hashtbl.find(rank_tbl, id);
    let rec idx = (i, ls) =>
      switch (ls) {
      | [] => 0
      | [l, ..._] when l == raw => i
      | [_, ...rest] => idx(i + 1, rest)
      };
    idx(0, used);
  };
  let n_ranks = max(1, List.length(used));

  PerfTimer.record("gl/rank", PerfTimer.now() -. t_gl);
  let t_gl = PerfTimer.now();
  /* ---- 2. order: barycenter sweeps ----
     Order state: per rank, node ids in display order. Neighbors from ALL
     edges (ranked or not) between adjacent-or-any ranks. */
  let columns: array(ref(list(string))) =
    Array.init(n_ranks, _ => ref([]));
  List.iter(
    id => columns[rank(id)] := (columns[rank(id)])^ @ [id],
    node_ids,
  );
  /* Edge endpoints that are attachments count as their (transitive)
     host for ordering/relaxation — a node whose only link is to some
     node's satellite should still gravitate toward that node. */
  let rec resolve_host = (~depth=0, id: string): option(string) =>
    if (is_node(id)) {
      Some(id);
    } else if (depth > 3) {
      None;
    } else {
      switch (List.find_opt((a: attachment) => a.id == id, spec.attachments)) {
      | Some(a) => resolve_host(~depth=depth + 1, a.host)
      | None => None
      };
    };
  /* adjacency once: the sweeps ask for every node's neighbors on every
     pass, and an edge scan per ask was the layout's whole cost at 100
     nodes (~30 ms a layout, ~25 layouts a tool call) */
  let adjacency: Hashtbl.t(string, list(string)) = Hashtbl.create(64);
  let add_neighbor = (id: string, other: option(string)) =>
    switch (other) {
    | Some(o) when o != id =>
      let cur =
        switch (Hashtbl.find_opt(adjacency, id)) {
        | Some(l) => l
        | None => []
        };
      Hashtbl.replace(adjacency, id, [o, ...cur]);
    | _ => ()
    };
  List.iter(
    (e: edge) => {
      let src = resolve_host(e.src)
      and dst = resolve_host(e.dst);
      switch (src) {
      | Some(s) => add_neighbor(s, dst)
      | None => ()
      };
      switch (dst) {
      | Some(d) => add_neighbor(d, src)
      | None => ()
      };
    },
    spec.edges,
  );
  let neighbors = (id: string): list(string) =>
    switch (Hashtbl.find_opt(adjacency, id)) {
    | Some(l) => List.rev(l)
    | None => []
    };
  /* display position within a column, refreshed after each column pass */
  let positions: Hashtbl.t(string, int) = Hashtbl.create(64);
  let index_column = (l: int): unit =>
    List.iteri((i, id) => Hashtbl.replace(positions, id, i), (columns[l])^);
  for (l in 0 to n_ranks - 1) {
    index_column(l);
  };
  let order_index = (id: string): option(float) =>
    Option.map(float_of_int, Hashtbl.find_opt(positions, id));
  let sweep = (~from_left: bool): unit => {
    let ranks_seq =
      from_left
        ? List.init(n_ranks, i => i) : List.init(n_ranks, i => n_ranks - 1 - i);
    List.iter(
      l => {
        let col = (columns[l])^;
        let keyed =
          List.mapi(
            (i, id) => {
              /* barycenter of neighbors in OTHER columns (the sweep
                 direction biases which ones have settled) */
              let ns =
                neighbors(id)
                |> List.filter(m => rank(m) != l)
                |> List.filter_map(order_index);
              let key =
                switch (ns) {
                | [] => float_of_int(i) /* keep position */
                | _ =>
                  List.fold_left((+.), 0., ns)
                  /. float_of_int(List.length(ns))
                };
              (key, float_of_int(i), id);
            },
            col,
          );
        /* damped adjacent exchange (see Spec.order_hysteresis): bubble
           passes swap a pair only on a clear barycenter win, so the
           result is the current order minus genuine inversions */
        let arr = Array.of_list(keyed);
        let n = Array.length(arr);
        let eps = spec.order_hysteresis;
        let swapped = ref(true);
        let passes = ref(0);
        while (swapped^ && passes^ < n) {
          swapped := false;
          incr(passes);
          for (i in 0 to n - 2) {
            let (k1, _, _) = arr[i]
            and (k2, _, _) = arr[i + 1];
            if (k1 > k2 +. eps) {
              let tmp = arr[i];
              arr[i] = arr[i + 1];
              arr[i + 1] = tmp;
              swapped := true;
            };
          };
        };
        columns[l] := Array.to_list(arr) |> List.map(((_, _, id)) => id);
        index_column(l);
      },
      ranks_seq,
    );
  };
  for (_ in 1 to spec.order_sweeps) {
    sweep(~from_left=true);
    sweep(~from_left=false);
  };

  /* ---- attachment extents: reserve room around hosts ---- */
  let att_extent_uncached = (id: string, side: side): float => {
    let matches = (p: side): bool =>
      switch (side, p) {
      | (Above, Above)
      | (Above, AboveLeft) => true /* diagonals still claim height */
      | (s, p) => s == p
      };
    spec.attachments
    |> List.filter((a: attachment) => a.host == id && matches(a.prefer))
    |> List.fold_left(
         (acc, a: attachment) => max(acc, a.dist +. a.radius *. 2.),
         0.,
       );
  };
  let node_extent_uncached = (id: string, side: side): float =>
    switch (List.find_opt((n: node) => n.id == id, spec.nodes)) {
    | Some(n) => side == Above ? n.extent_above : n.extent_below
    | None => 0.
    };
  let att_extent_memo: Hashtbl.t((string, side), float) =
    Hashtbl.create(64);
  let att_extent = (id: string, side: side): float =>
    switch (Hashtbl.find_opt(att_extent_memo, (id, side))) {
    | Some(v) => v
    | None =>
      let v = att_extent_uncached(id, side);
      Hashtbl.replace(att_extent_memo, (id, side), v);
      v;
    };
  let node_extent_memo: Hashtbl.t((string, side), float) =
    Hashtbl.create(64);
  let node_extent = (id: string, side: side): float =>
    switch (Hashtbl.find_opt(node_extent_memo, (id, side))) {
    | Some(v) => v
    | None =>
      let v = node_extent_uncached(id, side);
      Hashtbl.replace(node_extent_memo, (id, side), v);
      v;
    };

  PerfTimer.record("gl/order", PerfTimer.now() -. t_gl);
  let t_gl = PerfTimer.now();
  /* ---- 3. coords ---- */
  /* y_stretch spreads rows apart (gap scaling) rather than scaling
     positions: a flat band of rows would otherwise translate instead
     of spreading */
  let row_gap = spec.row_gap *. spec.y_stretch;
  let posed: Hashtbl.t(string, (pos, float)) = Hashtbl.create(16);
  /* column x-centers: gaps account for both columns' radii and any
     Out/In attachments hanging between them */
  let col_max_r = l =>
    List.fold_left((m, id) => max(m, radius(id)), 10., (columns[l])^);
  let col_side_extent = (l, side) =>
    List.fold_left(
      (m, id) => max(m, att_extent(id, side)),
      0.,
      (columns[l])^,
    );
  let x_centers = Array.make(n_ranks, spec.margin);
  for (l in 0 to n_ranks - 1) {
    x_centers[l] = (
      if (l == 0) {
        spec.margin +. col_max_r(0) +. col_side_extent(0, In);
      } else {
        x_centers[l - 1]
        +. col_max_r(l - 1)
        +. col_side_extent(l - 1, Out)
        +. spec.col_gap
        +. col_side_extent(l, In)
        +. col_max_r(l);
      }
    );
  };
  let x_center = l =>
    spec.margin +. (x_centers[l] -. spec.margin) *. spec.x_stretch;
  /* vertical slot: a node's height claim includes Above/Below extents */
  let v_half = (id: string): float =>
    radius(id)
    +. max(
         att_extent(id, Above) +. node_extent(id, Above),
         att_extent(id, Below) +. node_extent(id, Below),
       )
    /. 2.;
  let col_height = l =>
    switch ((columns[l])^) {
    | [] => 0.
    | col =>
      List.fold_left((acc, id) => acc +. v_half(id) *. 2., 0., col)
      +. float_of_int(List.length(col) - 1)
      *. row_gap
    };
  let max_h = List.fold_left(max, 0., List.init(n_ranks, col_height));
  /* initial stacked y, columns centered against the tallest */
  for (l in 0 to n_ranks - 1) {
    let y0 = spec.margin +. (max_h -. col_height(l)) /. 2.;
    let _ =
      List.fold_left(
        (y, id) => {
          let h = v_half(id);
          Hashtbl.replace(
            posed,
            id,
            (
              {
                x: x_center(l),
                y: y +. h,
              },
              radius(id),
            ),
          );
          y +. h *. 2. +. row_gap;
        },
        y0,
        (columns[l])^,
      );
    ();
  };
  /* y relaxation: pull nodes toward their neighbors' average y, then
     re-enforce order + gaps top-down. Keeps edges near-horizontal. */
  let relax_col = l => {
    let col = (columns[l])^;
    let targets =
      List.map(
        id => {
          let (p, _) = Hashtbl.find(posed, id);
          let ns =
            neighbors(id)
            |> List.filter(m => rank(m) != l)
            |> List.filter_map(m =>
                 Hashtbl.find_opt(posed, m) |> Option.map(((q, _)) => q.y)
               );
          switch (ns) {
          | [] => (id, p.y)
          | _ => (
              id,
              List.fold_left((+.), 0., ns) /. float_of_int(List.length(ns)),
            )
          };
        },
        col,
      );
    /* top-down gap enforcement over the target ys, in column order */
    let _ =
      List.fold_left(
        ((prev_bottom, _), (id, ty)) => {
          let h = v_half(id);
          let y = max(ty, prev_bottom +. h);
          let (p, r) = Hashtbl.find(posed, id);
          Hashtbl.replace(
            posed,
            id,
            (
              {
                x: p.x,
                y,
              },
              r,
            ),
          );
          (y +. h +. row_gap, ());
        },
        (spec.margin, ()),
        targets,
      );
    ();
  };
  for (_ in 1 to 2) {
    for (l in 0 to n_ranks - 1) {
      relax_col(l);
    };
    for (l in 0 to n_ranks - 1) {
      relax_col(n_ranks - 1 - l);
    };
  };

  /* ---- 3b. cross-column vertical relief ----
     Horizontal compression (x_stretch < 1) can leave neighboring
     columns' halos (node + docked attachments) overlapping in x; rows
     that overlap in both axes shear apart vertically. This is what
     turns tight graphs diagonal instead of cramming one band — extra
     height is created exactly where density demands it. */
  let h_halo = (id: string): float =>
    radius(id) +. (att_extent(id, In) +. att_extent(id, Out)) /. 2.;
  for (_ in 1 to 12) {
    List.iteri(
      (i, a) =>
        List.iteri(
          (j, b) =>
            if (j > i) {
              switch (
                Hashtbl.find_opt(posed, a),
                Hashtbl.find_opt(posed, b),
              ) {
              | (Some((pa, ra)), Some((pb, rb))) =>
                let need_x = h_halo(a) +. h_halo(b) +. 14.;
                let need_y = v_half(a) +. v_half(b) +. row_gap /. 2.;
                let dx = abs_float(pa.x -. pb.x)
                and dy = pb.y -. pa.y;
                if (dx < need_x && abs_float(dy) < need_y) {
                  let push = (need_y -. abs_float(dy)) /. 2.;
                  /* preserve current vertical order; ties break by
                     input order (a stays above) */
                  let s = dy > 0. || dy == 0. ? 1. : (-1.);
                  Hashtbl.replace(
                    posed,
                    a,
                    (
                      {
                        x: pa.x,
                        y: pa.y -. s *. push,
                      },
                      ra,
                    ),
                  );
                  Hashtbl.replace(
                    posed,
                    b,
                    (
                      {
                        x: pb.x,
                        y: pb.y +. s *. push,
                      },
                      rb,
                    ),
                  );
                };
              | _ => ()
              };
            },
          node_ids,
        ),
      node_ids,
    );
  };

  PerfTimer.record("gl/coords", PerfTimer.now() -. t_gl);
  let t_gl = PerfTimer.now();
  /* ---- 4. attachments: collision-aware ring search ---- */
  let collides = (p: pos, r: float): bool =>
    Hashtbl.fold(
      (_, (q, qr), acc) =>
        acc || Float.hypot(q.x -. p.x, q.y -. p.y) < r +. qr +. 6.,
      posed,
      false,
    );
  let base_angle = (s: side): float =>
    switch (s) {
    | In => 180.
    | Out => 0.
    | Above => 90.
    | AboveLeft => 135.
    | Below => 270.
    };
  /* In/Out attachment groups fan symmetrically about the horizontal:
     n same-side satellites of one host spread vertically (the dataflow
     fan-in / fan-out reading), rather than greedily claiming slots. */
  let fan_step = 32.;
  let group_total: Hashtbl.t((string, side), int) = Hashtbl.create(8);
  List.iter(
    (a: attachment) =>
      switch (a.prefer) {
      | In
      | Out =>
        let k = (a.host, a.prefer);
        Hashtbl.replace(
          group_total,
          k,
          1 + Option.value(~default=0, Hashtbl.find_opt(group_total, k)),
        );
      | _ => ()
      },
    spec.attachments,
  );
  let group_seen: Hashtbl.t((string, side), int) = Hashtbl.create(8);
  let fan_angle = (a: attachment): float => {
    let base = base_angle(a.prefer);
    switch (a.prefer) {
    | In
    | Out =>
      let k = (a.host, a.prefer);
      let n = Option.value(~default=1, Hashtbl.find_opt(group_total, k));
      let i = Option.value(~default=0, Hashtbl.find_opt(group_seen, k));
      Hashtbl.replace(group_seen, k, i + 1);
      /* In fans mirror (screen-y grows down; sin positive = up) */
      let dir = a.prefer == In ? (-1.) : 1.;
      base
      +. dir
      *. fan_step
      *. (float_of_int(i) -. float_of_int(n - 1) /. 2.);
    | _ => base
    };
  };
  /* candidate angles: preferred first, alternating outward in 30° steps */
  let angle_offsets = [0., 45., (-45.), 90., (-90.), 135., (-135.), 180.];
  let place_attachment = (a: attachment): bool =>
    switch (Hashtbl.find_opt(posed, a.id)) {
    | Some(_) => true
    | None =>
      switch (Hashtbl.find_opt(posed, a.host)) {
      | None => false
      | Some((hp, hr)) =>
        let a_base = fan_angle(a);
        let try_ring = (ring: int): option(pos) => {
          let dist =
            hr +. a.dist +. float_of_int(ring) *. (a.radius *. 2. +. 10.);
          List.fold_left(
            (found, off) =>
              switch (found) {
              | Some(_) => found
              | None =>
                let th = (a_base +. off) *. Float.pi /. 180.;
                let p = {
                  x: hp.x +. cos(th) *. dist,
                  /* screen y grows downward */
                  y: hp.y -. sin(th) *. dist,
                };
                collides(p, a.radius) ? None : Some(p);
              },
            None,
            angle_offsets,
          );
        };
        let p =
          switch (try_ring(0)) {
          | Some(p) => Some(p)
          | None =>
            switch (try_ring(1)) {
            | Some(p) => Some(p)
            | None => try_ring(2)
            }
          };
        switch (p) {
        | Some(p) =>
          Hashtbl.replace(posed, a.id, (p, a.radius));
          true;
        | None =>
          /* saturated neighborhood: overlap at the preferred slot rather
             than vanish */
          let th = a_base *. Float.pi /. 180.;
          let dist = hr +. a.dist;
          Hashtbl.replace(
            posed,
            a.id,
            (
              {
                x: hp.x +. cos(th) *. dist,
                y: hp.y -. sin(th) *. dist,
              },
              a.radius,
            ),
          );
          true;
        };
      }
    };
  /* host chains resolve across passes; anything still unhosted parks
     top-left rather than vanishing */
  let unresolved =
    spec.attachments
    |> List.filter(a => !place_attachment(a))
    |> List.filter(a => !place_attachment(a));
  List.iteri(
    (i, a: attachment) =>
      Hashtbl.replace(
        posed,
        a.id,
        (
          {
            x: spec.margin /. 2.,
            y: spec.margin /. 2. +. float_of_int(i) *. 30.,
          },
          a.radius,
        ),
      ),
    unresolved,
  );

  PerfTimer.record("gl/attach", PerfTimer.now() -. t_gl);
  /* ---- result (input order for determinism) ---- */
  let all_ids =
    node_ids @ List.map((a: attachment) => a.id, spec.attachments);
  {
    positions:
      List.filter_map(
        id =>
          Hashtbl.find_opt(posed, id) |> Option.map(((p, _)) => (id, p)),
        all_ids,
      ),
    ranks: List.map(id => (id, rank(id)), node_ids),
  };
};
