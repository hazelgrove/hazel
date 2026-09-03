open Util;
open Point;

module Point = Point;

[@deriving (show({with_path: false}), sexp, yojson)]
type measurement = {
  origin: Point.t,
  last: Point.t,
};

let mk_measurement = (origin: Point.t, last: Point.t): measurement => {
  origin,
  last,
};

module Rows = {
  include IntMap;
  type shape = {
    indent: col,
    max_col: col,
  };
  type t = IntMap.t(shape);

  let min_col = (rs: list(row), map: t) =>
    rs
    |> List.map(r => find(r, map).indent)
    |> List.fold_left(min, Int.max_int);
};

module Shards = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type shard = (int, measurement);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(shard);

  // elements of returned list are nonempty
  let rec split_by_row: t => list(t) =
    fun
    | [] => []
    | [hd, ...tl] =>
      switch (split_by_row(tl)) {
      | [] => [[hd]]
      | [row, ...rows] =>
        snd(List.hd(row)).origin.row == snd(hd).origin.row
          ? [[hd, ...row], ...rows] : [[hd], row, ...rows]
      };
};

/* the measurement of ONE CHUNK, rows counted from its own top */
type flat = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  secondary: Id.Map.t(measurement),
  projectors: Id.Map.t(measurement),
  rows: Rows.t,
  piece_rows: list(list(Piece.t)) /* NOTE: sublists are reversed */
};

let empty_flat = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  secondary: Id.Map.empty,
  projectors: Id.Map.empty,
  rows: Rows.empty,
  piece_rows: [],
};

let add_s = (id: Id.t, i: int, m, map) => {
  ...map,
  tiles:
    map.tiles
    |> Id.Map.update(
         id,
         fun
         | None => Some([(i, m)])
         | Some(ms) =>
           Some(
             [(i, m), ...ms]
             |> List.sort(((i, _), (j, _)) => Int.compare(i, j)),
           ),
       ),
};

let add_g = (g: Grout.t, m, map) => {
  ...map,
  grout: map.grout |> Id.Map.add(g.id, m),
};
let add_w = (w: Secondary.t, m, map) => {
  ...map,
  secondary: map.secondary |> Id.Map.add(w.id, m),
};
let add_pr = (p: Base.projector, m, map) => {
  ...map,
  projectors: map.projectors |> Id.Map.add(p.id, m),
};

let add_row = (row: int, shape: Rows.shape, map) => {
  ...map,
  rows: Rows.add(row, shape, map.rows),
};

let rec add_n_rows = (origin: Point.t, row_indent, n, map: flat): flat =>
  switch (n) {
  | 0 => map
  | _ =>
    map
    |> add_n_rows(origin, row_indent, n - 1)
    |> add_row(
         origin.row + n - 1,
         {
           indent: row_indent,
           max_col: origin.col,
         },
       )
  };

let add_piece_row = (_row: int, seg: list(Piece.t), map) => {
  ...map,
  piece_rows: [seg, ...map.piece_rows],
};

let add_empty_piece_rows = map => {
  ...map,
  piece_rows: [[], ...map.piece_rows],
};

let rec add_n_empty_piece_rows = (n: int, map) =>
  n <= 0 ? map : add_n_empty_piece_rows(n - 1, add_empty_piece_rows(map));

let find_shards_flat = (~msg="", t: Tile.t, map) =>
  try(Id.Map.find(t.id, map.tiles)) {
  | _ => failwith("find_shards: " ++ msg)
  };
let find_w_flat = (~msg="", w: Secondary.t, map: flat): measurement =>
  try(Id.Map.find(w.id, map.secondary)) {
  | _ => failwith("find_w: " ++ msg)
  };
let find_g_flat = (~msg="", g: Grout.t, map: flat): measurement =>
  try(Id.Map.find(g.id, map.grout)) {
  | _ => failwith("find_g: " ++ msg)
  };
let find_pr_flat = (~msg="", p: Base.projector, map: flat): measurement =>
  try(Id.Map.find(p.id, map.projectors)) {
  | _ => failwith("find_g: " ++ msg)
  };
let find_pr_opt_flat = (p: Base.projector, map: flat): option(measurement) =>
  Id.Map.find_opt(p.id, map.projectors);
// returns the measurement spanning the whole tile
let find_t_flat = (t: Tile.t, map: flat): measurement => {
  let shards = find_shards_flat(t, map);
  let (first, last) =
    try({
      let first = ListUtil.assoc_err(Tile.l_shard(t), shards, "find_t");
      let last = ListUtil.assoc_err(Tile.r_shard(t), shards, "find_t");
      (first, last);
    }) {
    | _ => failwith("find_t: inconsistent shard infor between tile and map")
    };
  {
    origin: first.origin,
    last: last.last,
  };
};
let find_p_flat = (~msg="", p: Piece.t, map: flat): measurement =>
  try(
    p
    |> Piece.get(
         w => find_w_flat(w, map),
         g => find_g_flat(g, map),
         t => find_t_flat(t, map),
         p => find_pr_flat(p, map),
       )
  ) {
  | _ => failwith("find_p: " ++ msg ++ "id: " ++ Id.to_string(p |> Piece.id))
  };

let find_by_id_flat = (id: Id.t, map: flat): option(measurement) => {
  switch (Id.Map.find_opt(id, map.secondary)) {
  | Some(m) => Some(m)
  | None =>
    switch (Id.Map.find_opt(id, map.grout)) {
    | Some(m) => Some(m)
    | None =>
      switch (Id.Map.find_opt(id, map.tiles)) {
      | Some(shards) =>
        let first =
          ListUtil.assoc_err(List.hd(shards) |> fst, shards, "find_by_id");
        let last =
          ListUtil.assoc_err(
            ListUtil.last(shards) |> fst,
            shards,
            "find_by_id",
          );
        Some({
          origin: first.origin,
          last: last.last,
        });
      | None =>
        switch (Id.Map.find_opt(id, map.projectors)) {
        | Some(m) => Some(m)
        | None =>
          Printf.printf(
            "Measured.WARNING: id %s not found",
            Id.to_string(id),
          );
          None;
        }
      }
    }
  };
};

type acc = (Segment.t, int, Point.t, flat);

module MkDeferredLinebreaks = () => {
  /* Tab projectors add linebreaks after the end of the line
     the begin on. This keeps track of these deffered linebreaks
     until the next (real) linebreak is reached */

  let lbs: ref(int) = ref(0);

  let consume = (): int => {
    let ret = lbs^;
    lbs := 0;
    ret;
  };

  let update = (num_lb: int): unit => lbs := max(num_lb, lbs^);

  let of_projector =
      (p: Base.projector, shape_map: Id.Map.t(ProjectorShape.t)): Point.t => {
    let shape = ProjectorCore.Shape.Map.lookup(p.id, shape_map);
    let row =
      switch (shape.vertical) {
      | Inline
      | Block(0) => 0
      | Tab(num_lb) =>
        update(num_lb);
        0;
      | Block(num_lb) => max(num_lb, consume())
      };
    {
      col: shape.horizontal,
      row,
    };
  };

  let of_secondary = (): int => 1 + consume();
};

let of_segment_inner =
    (
      ~final: bool,
      indent_level: Id.Map.t(int),
      is_single_line: bool,
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorCore.Shape.t),
      refractor_shape_map: Id.Map.t(int),
    )
    : flat => {
  module DeferredLinebreaks = MkDeferredLinebreaks();

  let indent_level =
    Id.Map.is_empty(indent_level) && !is_single_line
      ? Indentation.level_map(seg) : indent_level;

  let indent_of_linebreak = (w: Secondary.t): option(int) =>
    Secondary.is_linebreak(w) ? Id.Map.find_opt(w.id, indent_level) : None;

  let calc = (indent: int, origin: Point.t, map: flat, size: Point.t) => {
    let last = Point.add(origin, size);
    let map = add_n_rows(origin, indent, size.row, map);
    (mk_measurement(origin, last), map);
  };

  let shardify = (t: Tile.t, idx: int): Tile.t => {
    {
      ...t,
      shards: [idx],
      children: [],
    };
  };

  let add_shard = ((seg, indent, origin, map): acc, t: Tile.t, idx: int) => {
    let size = Token.bounding_box(List.nth(t.label, idx));
    let (measure, map) = calc(indent, origin, map, size);
    (
      [Piece.Tile(shardify(t, idx)), ...seg],
      indent,
      measure.last,
      add_s(t.id, idx, measure, map),
    );
  };

  let add_grout = ((seg, indent, origin, map): acc, g: Grout.t) => {
    let size = Point.mk(~row=0, ~col=1);
    let (measure, map) = calc(indent, origin, map, size);
    (
      [Piece.Grout(g), ...seg],
      indent,
      measure.last,
      add_g(g, measure, map),
    );
  };

  let add_projector = ((seg, indent, origin, map): acc, pr: Base.projector) => {
    let size = DeferredLinebreaks.of_projector(pr, shape_map);
    let shape = ProjectorCore.Shape.Map.lookup(pr.id, shape_map);
    let indent =
      switch (shape.vertical) {
      | Inline
      | Block(0)
      | Tab(_) => indent
      | Block(_) => origin.col
      };
    let (measure, map) = calc(indent, origin, map, size);
    let map =
      size.row == 0
        ? map
        : add_piece_row(origin.row, [Piece.Projector(pr), ...seg], map);
    let map = size.row == 0 ? map : add_n_empty_piece_rows(size.row - 1, map);
    let seg = size.row == 0 ? [Piece.Projector(pr), ...seg] : [];
    (seg, indent, measure.last, add_pr(pr, measure, map));
  };

  let add_secondary = ((seg, prev_indent, origin, map): acc, w: Secondary.t) => {
    let (seg, new_indent, size, map) =
      switch (indent_of_linebreak(w)) {
      | Some(new_indent) =>
        let size =
          Point.mk(
            ~row=DeferredLinebreaks.of_secondary(),
            ~col=new_indent - origin.col,
          );
        // add seg to map and reset seg
        let map =
          add_piece_row(
            origin.row,
            seg @ [Piece.Secondary(Secondary.mk_newline(Id.mk()))], /* NOTE: These linebreaks don't actually occur in the surface syntax */
            map,
          );
        let map =
          size.row == 0 ? map : add_n_empty_piece_rows(size.row - 1, map);
        ([], new_indent, size, map);
      | None =>
        let size = Point.mk(~row=0, ~col=Secondary.columns(w));
        ([Piece.Secondary(w), ...seg], prev_indent, size, map);
      };
    let (measure, map) = calc(prev_indent, origin, map, size);
    (seg, new_indent, measure.last, add_w(w, measure, map));
  };

  let add_top_level = ((seg, indent, origin, map): acc, ~top_level: bool) => {
    let map =
      top_level
        ? {
          let g = DeferredLinebreaks.of_secondary();
          add_n_rows(origin, indent, g, map)
          |> add_piece_row(
               origin.row,
               seg @ [Piece.Secondary(Secondary.mk_newline(Id.mk()))], /* NOTE: These linebreaks don't actually occur in the surface syntax */
               _,
             )
          |> add_n_empty_piece_rows(g - 1);
        }
        : map;
    (seg, indent, origin, map);
  };

  let rec go = (~top_level: bool, acc: acc, seg: Segment.t): acc =>
    switch (seg) {
    | [] => add_top_level(~top_level, acc)
    | [hd, ...tl] => go(~top_level, of_piece(acc, hd), tl)
    }
  and of_piece = (acc: acc, p: Piece.t): acc =>
    switch (p) {
    | Secondary(w) => add_secondary(acc, w)
    | Grout(g) => add_grout(acc, g)
    | Projector(p) => add_projector(acc, p)
    | Tile(t) =>
      switch (Id.Map.find_opt(t.id, refractor_shape_map)) {
      | Some(_) =>
        DeferredLinebreaks.update(2) |> ignore;
        ();
      | None => ()
      };
      Aba.fold_left(
        add_shard(acc, t),
        (acc, seg) => add_shard(go(~top_level=false, acc, seg), t),
        Aba.mk(t.shards, t.children),
      );
    };
  let (_, _, _, map) =
    go(~top_level=final, ([], 0, Point.zero, empty_flat), seg);
  map;
};

/* ===== CHUNKED MEASUREMENT (plans/subeditor-dataflow.md §5a) =====
   The program is measured PER TOP-LEVEL CHUNK (item runs cut only
   where a boundary is followed by a linebreak, so every chunk is a
   whole-lines block starting at column 0) and composed by row
   offsets. An edit re-measures one chunk; unchanged chunks are
   pointer-identical and reuse their measurements. Queries translate
   at lookup time; parity with the monolithic measurement is
   test-gated ([flatten] below exists for that). */

type chunk = {
  c_anchor: Id.t, /* first piece's id: the chunk's stable identity */
  c_start: int, /* absolute starting row */
  c_height: int, /* flat_height(c_flat), cached: chunk_for_row runs on
                    every row_shape and a per-probe max_binding_opt
                    made row-sweeping decorations quadratic-ish */
  c_pieces: Segment.t, /* the chunk's top-level pieces (for chunked views) */
  c_flat: flat,
};

type t = {
  chunks: array(chunk),
  /* piece id -> owning chunk ANCHOR (anchors are stable across
     partition changes; indices are not). Persistent snapshot per
     value: retained old generations keep answering correctly. */
  chunk_of_id: Id.Map.t(Id.t),
  /* anchor -> index in [chunks] (rebuilt O(#chunks) per generation) */
  anchor_index: Hashtbl.t(Id.t, int),
  total_rows: int,
  /* EAGER: a lazy thunk here is a functional value and breaks
     structural compares of anything containing a measurement */
  all_piece_rows: list(list(Piece.t)),
};

let flat_height = (f: flat): int =>
  switch (Rows.max_binding_opt(f.rows)) {
  | Some((r, _)) => r + 1
  | None => 0
  };

let ids_of_flat = (f: flat): list(Id.t) =>
  List.map(fst, Id.Map.bindings(f.tiles))
  @ List.map(fst, Id.Map.bindings(f.grout))
  @ List.map(fst, Id.Map.bindings(f.secondary))
  @ List.map(fst, Id.Map.bindings(f.projectors));

let shift_point = (s: int, p: Point.t): Point.t => {
  ...p,
  row: p.row + s,
};
let shift_m = (s: int, m: measurement): measurement => {
  origin: shift_point(s, m.origin),
  last: shift_point(s, m.last),
};

let mk_chunked = (~chunk_of_id, flats: list((Id.t, Segment.t, flat))): t => {
  let n = List.length(flats);
  let anchor_index = Hashtbl.create(n > 0 ? n : 1);
  let (chunks_rev, total) =
    List.fold_left(
      ((acc, row), (anchor, pieces, f)) => {
        Hashtbl.replace(anchor_index, anchor, List.length(acc));
        let h = flat_height(f);
        (
          [
            {
              c_anchor: anchor,
              c_start: row,
              c_height: h,
              c_pieces: pieces,
              c_flat: f,
            },
            ...acc,
          ],
          row + h,
        );
      },
      ([], 0),
      flats,
    );
  let chunks = Array.of_list(List.rev(chunks_rev));
  {
    chunks,
    chunk_of_id,
    anchor_index,
    total_rows: total,
    all_piece_rows:
      Array.fold_left((acc, ch) => ch.c_flat.piece_rows @ acc, [], chunks),
  };
};

let chunk_for_id = (id: Id.t, m: t): option(chunk) =>
  switch (Id.Map.find_opt(id, m.chunk_of_id)) {
  | None => None
  | Some(anchor) =>
    switch (Hashtbl.find_opt(m.anchor_index, anchor)) {
    | Some(i) => Some(m.chunks[i])
    | None => None
    }
  };

let chunk_for_row = (row: int, m: t): option(chunk) => {
  let n = Array.length(m.chunks);
  let rec bs = (lo, hi) =>
    if (lo > hi) {
      None;
    } else {
      let mid = (lo + hi) / 2;
      let ch = m.chunks[mid];
      let h = ch.c_height;
      if (row < ch.c_start) {
        bs(lo, mid - 1);
      } else if (row >= ch.c_start + h && mid < n - 1) {
        bs(mid + 1, hi);
      } else {
        Some(ch);
      };
    };
  n == 0 ? None : bs(0, n - 1);
};

/* ---- public accessors (chunk-translated) ---- */

let find_shards = (~msg="", t: Tile.t, m: t) =>
  switch (chunk_for_id(t.id, m)) {
  | Some(ch) =>
    find_shards_flat(~msg, t, ch.c_flat)
    |> List.map(((i, meas)) => (i, shift_m(ch.c_start, meas)))
  | None => failwith("find_shards: " ++ msg)
  };

let find_w = (~msg="", w: Secondary.t, m: t): measurement =>
  switch (chunk_for_id(w.id, m)) {
  | Some(ch) => shift_m(ch.c_start, find_w_flat(~msg, w, ch.c_flat))
  | None => failwith("find_w: " ++ msg)
  };
let find_g = (~msg="", g: Grout.t, m: t): measurement =>
  switch (chunk_for_id(g.id, m)) {
  | Some(ch) => shift_m(ch.c_start, find_g_flat(~msg, g, ch.c_flat))
  | None => failwith("find_g: " ++ msg)
  };
let find_pr = (~msg="", p: Base.projector, m: t): measurement =>
  switch (chunk_for_id(p.id, m)) {
  | Some(ch) => shift_m(ch.c_start, find_pr_flat(~msg, p, ch.c_flat))
  | None => failwith("find_pr: " ++ msg)
  };
let find_pr_opt = (p: Base.projector, m: t): option(measurement) =>
  switch (chunk_for_id(p.id, m)) {
  | Some(ch) =>
    find_pr_opt_flat(p, ch.c_flat) |> Option.map(shift_m(ch.c_start))
  | None => None
  };
let find_t = (t: Tile.t, m: t): measurement =>
  switch (chunk_for_id(t.id, m)) {
  | Some(ch) => shift_m(ch.c_start, find_t_flat(t, ch.c_flat))
  | None => failwith("find_t")
  };
let find_p = (~msg="", p: Piece.t, m: t): measurement =>
  switch (chunk_for_id(Piece.id(p), m)) {
  | Some(ch) => shift_m(ch.c_start, find_p_flat(~msg, p, ch.c_flat))
  | None =>
    failwith("find_p: " ++ msg ++ "id: " ++ Id.to_string(p |> Piece.id))
  };
let find_by_id = (id: Id.t, m: t): option(measurement) =>
  switch (chunk_for_id(id, m)) {
  | Some(ch) =>
    find_by_id_flat(id, ch.c_flat) |> Option.map(shift_m(ch.c_start))
  | None =>
    Printf.printf("Measured.WARNING: id %s not found", Id.to_string(id));
    None;
  };

let find_shards_by_id = (id: Id.t, m: t): option(Shards.t) =>
  switch (chunk_for_id(id, m)) {
  | Some(ch) =>
    Id.Map.find_opt(id, ch.c_flat.tiles)
    |> Option.map(List.map(((i, meas)) => (i, shift_m(ch.c_start, meas))))
  | None => None
  };

let row_shape = (row: int, m: t): option(Rows.shape) =>
  switch (chunk_for_row(row, m)) {
  | Some(ch) => Rows.find_opt(row - ch.c_start, ch.c_flat.rows)
  | None => None
  };

let row_indent = (row: int, m: t): int =>
  switch (row_shape(row, m)) {
  | Some(sh) => sh.indent
  | None => 0
  };

let min_col_of_rows = (rs: list(row), m: t): col =>
  rs
  |> List.map(r =>
       switch (row_shape(r, m)) {
       | Some(sh) => sh.indent
       | None => Int.max_int
       }
     )
  |> List.fold_left(min, Int.max_int);

let piece_rows = (m: t): list(list(Piece.t)) => m.all_piece_rows;

let num_rows = (m: t): int => m.total_rows;

/* single-chunk construction: the compatibility path every existing
   of_segment caller keeps using */
let of_segment =
    (
      ~indent_level=Id.Map.empty,
      ~is_single_line=false,
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorCore.Shape.t),
      refractor_shape_map: Id.Map.t(int),
    )
    : t => {
  let f =
    of_segment_inner(
      ~final=true,
      indent_level,
      is_single_line,
      seg,
      shape_map,
      refractor_shape_map,
    );
  let anchor =
    switch (seg) {
    | [p, ..._] => Piece.id(p)
    | [] => Id.invalid
    };
  let chunk_of_id =
    List.fold_left(
      (acc, id) => Id.Map.add(id, anchor, acc),
      Id.Map.empty,
      ids_of_flat(f),
    );
  mk_chunked(~chunk_of_id, [(anchor, seg, f)]);
};

let empty: t = mk_chunked(~chunk_of_id=Id.Map.empty, []);

/* translate-and-union: TEST-ONLY parity target vs a monolithic
   measurement */
let flatten = (m: t): flat =>
  Array.fold_left(
    (acc, ch) => {
      let s = ch.c_start;
      let f = ch.c_flat;
      {
        tiles:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.tiles,
            Id.Map.map(
              List.map(((i, ms)) => (i, shift_m(s, ms))),
              f.tiles,
            ),
          ),
        grout:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.grout,
            Id.Map.map(shift_m(s), f.grout),
          ),
        secondary:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.secondary,
            Id.Map.map(shift_m(s), f.secondary),
          ),
        projectors:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.projectors,
            Id.Map.map(shift_m(s), f.projectors),
          ),
        rows:
          Rows.union(
            (_, _, y) => Some(y),
            acc.rows,
            f.rows
            |> Rows.bindings
            |> List.map(((r, sh)) => (r + s, sh))
            |> List.to_seq
            |> Rows.of_seq,
          ),
        piece_rows: f.piece_rows @ acc.piece_rows,
      };
    },
    empty_flat,
    m.chunks,
  );

/* Width in characters of row at measurement.origin */
let start_row_width = (measurement: measurement, measured: t): int =>
  switch (row_shape(measurement.origin.row, measured)) {
  | None => 0
  | Some(row) => row.max_col
  };

/* ===== INCREMENTAL CHUNKED BUILDER =====
   Partition the top-level piece list at linebreaks where the
   indentation walk (Indentation.go') is provably back in its initial
   state, so measuring each run standalone equals the monolithic
   measurement. Per-chunk results are memoized by anchor: an edit
   re-measures only the chunks whose pieces (or projector/refractor
   shape slices) changed. Exact parity with the monolithic build is
   test-gated (Test_MeasuredChunks). */
module Incr = {
  type entry = {
    e_pieces: Segment.t,
    e_final: bool,
    e_flat: flat,
    e_ids: list(Id.t),
    /* shape-map/refractor bindings landing in this chunk, in
       descending id order (both writers cons over an ascending
       iteration) — a projector changing shape re-measures its chunk */
    mutable e_shape_slice: list((Id.t, ProjectorCore.Shape.t)),
    mutable e_refr_slice: list((Id.t, int)),
  };

  /* One cache per editor (rides in CachedSyntax): [prev] is the last
     build's id->anchor map + per-anchor entries. Eviction: entries
     for anchors absent from the current partition are dropped every
     build (retained old Measured.t generations carry their own
     snapshots and never consult the cache). */
  type cache = {
    mutable prev: option((Id.Map.t(Id.t), Hashtbl.t(Id.t, entry))),
  };
  let mk_cache = (): cache => {prev: None};

  /* telemetry/test hooks: chunks re-measured vs reused, cumulative */
  let built = ref(0);
  let reused = ref(0);

  let rec seg_ptr_eq = (a: Segment.t, b: Segment.t): bool =>
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) => x === y && seg_ptr_eq(xs, ys)
    | _ => false
    };

  /* Where may we cut? Immediately after a linebreak L such that:
     - L is the LAST linebreak of its run of consecutive secondaries
       and a non-secondary piece follows (chunks end in a linebreak:
       linebreaks drain the deferred-linebreak queue so no deferral
       crosses the cut, and the accumulating piece-row is flushed;
       post-linebreak spaces/comments spill to the next chunk);
     - the level Indentation.go' assigns L is 0 (L's indent positions
       the next chunk's first row, which standalone starts at col 0);
     - completion (Indentation.complete_segment) does not swallow
       across the cut: from the first incomplete top-level tile,
       cutting is suppressed up to the blank line bounding the
       completion — or to segment end if a later incomplete would be
       left raw globally (a standalone chunk would complete it). */
  let partition = (seg: Segment.t): list((Id.t, Segment.t, bool)) =>
    switch (seg) {
    | [] => [(Id.invalid, [], true)]
    | _ =>
      let ps = Array.of_list(seg);
      let n = Array.length(ps);
      let is_content = (p: Piece.t) =>
        switch (p) {
        | Grout({shape: Concave, _}) => false
        | Secondary(s) => !Secondary.is_space(s)
        | _ => true
        };
      let is_lb = (p: Piece.t) =>
        switch (p) {
        | Secondary(s) => Secondary.is_linebreak(s)
        | _ => false
        };
      let is_sec = (p: Piece.t) =>
        switch (p) {
        | Secondary(_) => true
        | _ => false
        };
      /* nearest trimmed-content index strictly before/after each i */
      let prev_ci = Array.make(n, -1);
      let next_ci = Array.make(n, -1);
      let last = ref(-1);
      for (i in 0 to n - 1) {
        prev_ci[i] = last^;
        if (is_content(ps[i])) {
          last := i;
        };
      };
      last := (-1);
      for (i in n - 1 downto 0) {
        next_ci[i] = last^;
        if (is_content(ps[i])) {
          last := i;
        };
      };
      let incomplete = i =>
        switch (ps[i]) {
        | Tile(t) => !Tile.is_complete(t)
        | _ => false
        };
      let first_inc = ref(-1);
      for (i in n - 1 downto 0) {
        if (incomplete(i)) {
          first_inc := i;
        };
      };
      /* first blank line (adjacent trimmed linebreaks) after the
         first incomplete tile = where completion's swallow stops */
      let blank_w1 = ref(-1);
      if (first_inc^ >= 0) {
        for (i in first_inc^ + 1 to n - 1) {
          if (blank_w1^ < 0
              && is_lb(ps[i])
              && next_ci[i] >= 0
              && is_lb(ps[next_ci[i]])) {
            blank_w1 := i;
          };
        };
      };
      let inc_after_w1 = {
        let found = ref(false);
        if (blank_w1^ >= 0) {
          for (i in blank_w1^ + 1 to n - 1) {
            if (incomplete(i)) {
              found := true;
            };
          };
        };
        found^;
      };
      /* cuts at indices in [suppress_lo, suppress_hi] are forbidden
         (and their linebreaks are inside the completed tile's
         children, so they don't update the top-level level) */
      let (suppress_lo, suppress_hi) =
        if (first_inc^ < 0) {
          (n, n);
        } else if (blank_w1^ >= 0 && !inc_after_w1) {
          (first_inc^, blank_w1^);
        } else {
          (first_inc^, n);
        };
      /* the tile completion substitutes for the region: full label
         (only its label/mold matter to the level rules) */
      let completed_prev: option(Piece.t) =
        suppress_lo < n
          ? switch (ps[suppress_lo]) {
            | Tile(t) =>
              Some(
                Tile({
                  ...t,
                  shards: List.init(List.length(t.label), x => x),
                }),
              )
            | _ => None
            }
          : None;
      /* one linebreak-level step of Indentation.go' at top level
         (not_top=false, base=0), guards in source order */
      let rule = (pv: option(Piece.t), nx: option(Piece.t), level: int): int =>
        switch (pv, nx) {
        | (_, Some(p)) when Indentation.is_comma(p) => 2
        | (Some(p), _) when Indentation.is_comma(p) => 2
        | (Some(p), _) when Indentation.is_incrementor(p) => level + 2
        | (_, Some(p)) when Indentation.is_case_rule(p) => 0
        | (_, None) => 0
        | (_, Some(p)) when Piece.is_infix_delimiter_op_prefix(p) => 0
        | (_, Some(_)) => level
        };
      /* last linebreak of its secondary run, with content after? */
      let rec run_ends_here = k =>
        k >= n
          ? false
          : is_lb(ps[k])
              ? false : is_sec(ps[k]) ? run_ends_here(k + 1) : true;
      let cuts = ref([]);
      let level = ref(0);
      for (i in 0 to n - 1) {
        if (is_lb(ps[i]) && !(i >= suppress_lo && i <= suppress_hi)) {
          let pv =
            switch (prev_ci[i]) {
            | j when j >= suppress_lo && j <= suppress_hi => completed_prev
            | j when j >= 0 => Some(ps[j])
            | _ => None
            };
          let nx = next_ci[i] >= 0 ? Some(ps[next_ci[i]]) : None;
          level := rule(pv, nx, level^);
          if (level^ == 0 && run_ends_here(i + 1)) {
            cuts := [i, ...cuts^];
          };
        };
      };
      let sub = (lo, hi) => Array.to_list(Array.sub(ps, lo, hi - lo + 1));
      let rec take = (lo, cs, acc) =>
        switch (cs) {
        | [] =>
          List.rev([(Piece.id(ps[lo]), sub(lo, n - 1), true), ...acc])
        | [c, ...cs] =>
          take(c + 1, cs, [(Piece.id(ps[lo]), sub(lo, c), false), ...acc])
        };
      take(0, List.rev(cuts^), []);
    };

  /* group bindings by owning anchor under [map] (descending id order
     per anchor: cons over Id.Map's ascending iteration) */
  let slices_of =
      (map: Id.Map.t(Id.t), bindings: list((Id.t, 'a)))
      : Hashtbl.t(Id.t, list((Id.t, 'a))) => {
    let h = Hashtbl.create(8);
    List.iter(
      ((id, v)) =>
        switch (Id.Map.find_opt(id, map)) {
        | Some(anchor) =>
          let cur =
            switch (Hashtbl.find_opt(h, anchor)) {
            | Some(l) => l
            | None => []
            };
          Hashtbl.replace(h, anchor, [(id, v), ...cur]);
        | None => ()
        },
      bindings,
    );
    h;
  };

  let of_segment =
      (
        ~cache: cache,
        seg: Segment.t,
        shape_map: Id.Map.t(ProjectorCore.Shape.t),
        refractor_shape_map: Id.Map.t(int),
      )
      : t => {
    let parts = partition(seg);
    let (prev_map, prev_entries) =
      switch (cache.prev) {
      | Some((m, e)) => (m, e)
      | None => (Id.Map.empty, Hashtbl.create(1))
      };
    let shape_slices = slices_of(prev_map, Id.Map.bindings(shape_map));
    let refr_slices =
      slices_of(prev_map, Id.Map.bindings(refractor_shape_map));
    let slice_for = (h, anchor) =>
      switch (Hashtbl.find_opt(h, anchor)) {
      | Some(l) => l
      | None => []
      };
    let new_entries = Hashtbl.create(List.length(parts));
    let chunks =
      List.map(
        ((anchor, pieces, final)) => {
          let e =
            switch (Hashtbl.find_opt(prev_entries, anchor)) {
            | Some(e)
                when
                  e.e_final == final
                  && seg_ptr_eq(e.e_pieces, pieces)
                  && e.e_shape_slice == slice_for(shape_slices, anchor)
                  && e.e_refr_slice == slice_for(refr_slices, anchor) =>
              incr(reused);
              e;
            | _ =>
              incr(built);
              let f =
                of_segment_inner(
                  ~final,
                  Id.Map.empty,
                  false,
                  pieces,
                  shape_map,
                  refractor_shape_map,
                );
              {
                e_pieces: pieces,
                e_final: final,
                e_flat: f,
                e_ids: ids_of_flat(f),
                e_shape_slice: [], /* filled below from the new map */
                e_refr_slice: [],
              };
            };
          Hashtbl.replace(new_entries, anchor, e);
          (anchor, e);
        },
        parts,
      );
    /* chunk_of_id as a diff on the previous generation's map: first
       remove the ids of every anchor that vanished or was rebuilt,
       then add the rebuilt chunks' ids — O(changed ids), and no dead
       ids linger (removal covers exactly what additions had added) */
    let map = ref(prev_map);
    Hashtbl.iter(
      (anchor, old_e: entry) =>
        switch (Hashtbl.find_opt(new_entries, anchor)) {
        | Some(e) when e === old_e => ()
        | _ => List.iter(id => map := Id.Map.remove(id, map^), old_e.e_ids)
        },
      prev_entries,
    );
    List.iter(
      ((anchor, e: entry)) => {
        let carried =
          switch (Hashtbl.find_opt(prev_entries, anchor)) {
          | Some(old_e) => old_e === e
          | None => false
          };
        if (!carried) {
          List.iter(id => map := Id.Map.add(id, anchor, map^), e.e_ids);
        };
      },
      chunks,
    );
    let chunk_of_id = map^;
    List.iter(
      ((_, e: entry)) => {
        e.e_shape_slice = [];
        e.e_refr_slice = [];
      },
      chunks,
    );
    Id.Map.iter(
      (id, sh) =>
        switch (Id.Map.find_opt(id, chunk_of_id)) {
        | Some(a) =>
          switch (Hashtbl.find_opt(new_entries, a)) {
          | Some(e) => e.e_shape_slice = [(id, sh), ...e.e_shape_slice]
          | None => ()
          }
        | None => ()
        },
      shape_map,
    );
    Id.Map.iter(
      (id, v) =>
        switch (Id.Map.find_opt(id, chunk_of_id)) {
        | Some(a) =>
          switch (Hashtbl.find_opt(new_entries, a)) {
          | Some(e) => e.e_refr_slice = [(id, v), ...e.e_refr_slice]
          | None => ()
          }
        | None => ()
        },
      refractor_shape_map,
    );
    cache.prev = Some((chunk_of_id, new_entries));
    mk_chunked(
      ~chunk_of_id,
      List.map(((a, e: entry)) => (a, e.e_pieces, e.e_flat), chunks),
    );
  };
};
