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
  let node_ids = List.map((n: node) => n.id, spec.nodes);
  let is_node = (id: string) => List.mem(id, node_ids);
  let radius = (id: string): float =>
    switch (List.find_opt((n: node) => n.id == id, spec.nodes)) {
    | Some(n) => n.radius
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
  let neighbors = (id: string): list(string) =>
    List.filter_map(
      (e: edge) => {
        let other =
          if (e.src == id) {
            resolve_host(e.dst);
          } else if (e.dst == id) {
            resolve_host(e.src);
          } else {
            None;
          };
        switch (other) {
        | Some(o) when o != id => Some(o)
        | _ => None
        };
      },
      spec.edges,
    );
  let order_index = (id: string): option(float) => {
    let col = (columns[rank(id)])^;
    let rec find = (i, ls) =>
      switch (ls) {
      | [] => None
      | [x, ..._] when x == id => Some(float_of_int(i))
      | [_, ...rest] => find(i + 1, rest)
      };
    find(0, col);
  };
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
        let sorted =
          List.stable_sort(
            ((k1, i1, _), (k2, i2, _)) =>
              switch (compare(k1, k2)) {
              | 0 => compare(i1, i2)
              | c => c
              },
            keyed,
          );
        columns[l] := List.map(((_, _, id)) => id, sorted);
      },
      ranks_seq,
    );
  };
  for (_ in 1 to spec.order_sweeps) {
    sweep(~from_left=true);
    sweep(~from_left=false);
  };

  /* ---- attachment extents: reserve room around hosts ---- */
  let att_extent = (id: string, side: side): float =>
    spec.attachments
    |> List.filter((a: attachment) => a.host == id && a.prefer == side)
    |> List.fold_left(
         (acc, a: attachment) => max(acc, a.dist +. a.radius *. 2.),
         0.,
       );

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
    radius(id) +. max(att_extent(id, Above), att_extent(id, Below)) /. 2.;
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
    | Below => 270.
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
        let try_ring = (ring: int): option(pos) => {
          let dist =
            hr +. a.dist +. float_of_int(ring) *. (a.radius *. 2. +. 10.);
          List.fold_left(
            (found, off) =>
              switch (found) {
              | Some(_) => found
              | None =>
                let th = (base_angle(a.prefer) +. off) *. Float.pi /. 180.;
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
          let th = base_angle(a.prefer) *. Float.pi /. 180.;
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
