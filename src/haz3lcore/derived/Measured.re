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
  /* content_start: column of first non-whitespace piece on row
   * content_end: column after last non-whitespace piece on row
   * max_col: absolute rightmost column (including whitespace)
   * For all-whitespace rows: content_start = max_col, content_end = 0 */
  type shape = {
    content_start: col,
    content_end: col,
    max_col: col,
  };
  type t = IntMap.t(shape);

  let min_content_start = (rs: list(row), map: t) =>
    rs
    |> List.map(r => find(r, map).content_start)
    |> List.fold_left(min, Int.max_int);

  let max_content_end = (rs: list(row), map: t) =>
    rs |> List.map(r => find(r, map).content_end) |> List.fold_left(max, 0);
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

/* Intrinsic geometric info about a splice. The splice's content is
 * measured in its own coordinate frame (origin = (0,0)); the [size] is
 * the bounding box of that content. Pieces inside the splice are stored
 * in the top-level [t] maps with their splice-local coordinates (piece
 * ids are globally unique so this does not collide with outer pieces). */
[@deriving (show({with_path: false}), sexp, yojson)]
type splice_info = {size: Point.t};

/* the measurement of ONE CHUNK, rows counted from its own top */
type flat = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  secondary: Id.Map.t(measurement),
  projectors: Id.Map.t(measurement),
  /* Intrinsic size and parent info for each splice, keyed by splice id. */
  splices: Id.Map.t(splice_info),
  rows: Rows.t,
  piece_rows: list(list(Piece.t)) /* NOTE: sublists are reversed */
};

let empty_flat = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  secondary: Id.Map.empty,
  projectors: Id.Map.empty,
  splices: Id.Map.empty,
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

let add_splice_info = (s: Base.splice, info: splice_info, map) => {
  ...map,
  splices: map.splices |> Id.Map.add(s.id, info),
};

/* Merge the splice-local inner map (measurements of pieces inside a splice)
 * into the top-level map. Splice-local pieces are stored with their local
 * coordinates; because piece ids are globally unique, these do not
 * collide with outer entries. [rows] and [piece_rows] from the inner
 * map are discarded (they belong to the splice's coordinate frame). */
let merge_inner = (inner: flat, outer: flat): flat => {
  let join = (a, b) => Id.Map.union((_, _, v2) => Some(v2), a, b);
  {
    tiles: join(outer.tiles, inner.tiles),
    grout: join(outer.grout, inner.grout),
    secondary: join(outer.secondary, inner.secondary),
    projectors: join(outer.projectors, inner.projectors),
    splices: join(outer.splices, inner.splices),
    rows: outer.rows,
    piece_rows: outer.piece_rows,
  };
};

let add_row = (row: int, shape: Rows.shape, map) => {
  ...map,
  rows: Rows.add(row, shape, map.rows),
};

let rec add_n_rows = (origin: Point.t, shape: Rows.shape, n, map: flat): flat =>
  switch (n) {
  | 0 => map
  | _ =>
    map
    |> add_n_rows(origin, shape, n - 1)
    |> add_row(origin.row + n - 1, shape)
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
/* A splice consumes zero width in its parent's coordinate frame; its
 * intrinsic size is recorded separately in [map.splices] and the splice's
 * actual on-screen placement is decided by its parent projector's view. */
let find_splice_placeholder_flat = (s: Base.splice, map: flat): measurement => {
  let origin =
    switch (Id.Map.find_opt(s.id, map.grout)) {
    | Some(m) => m.origin
    | None => Point.zero
    };
  {
    origin,
    last: origin,
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
         s => find_splice_placeholder_flat(s, map),
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
      | None => Id.Map.find_opt(id, map.projectors)
      }
    }
  };
};

/* Content bounds of the row currently being measured */
type row_content_ = {
  start_opt: option(int), /* column of first non-whitespace, None if none yet */
  end_col: int /* column after last non-whitespace */
};

type measure_acc = {
  seg: Segment.t, /* pieces accumulated on current row (reversed) */
  pos: Point.t,
  map: flat,
  row_content: row_content_,
};

let empty_row_content_: row_content_ = {
  start_opt: None,
  end_col: 0,
};

/* Extend content bounds; call only for non-whitespace pieces */
let update_row_content_ =
    (rc: row_content_, origin: Point.t, size: Point.t): row_content_ => {
  let col = origin.col;
  let end_col = col + size.col;
  {
    start_opt:
      switch (rc.start_opt) {
      | None => Some(col)
      | Some(c) => Some(min(c, col))
      },
    end_col: max(rc.end_col, end_col),
  };
};

let shape_of_row_content_ = (rc: row_content_, max_col: int): Rows.shape => {
  content_start:
    switch (rc.start_opt) {
    | Some(c) => c
    | None => max_col /* all whitespace row */
    },
  content_end: rc.end_col,
  max_col,
};

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
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorCore.Shape.t),
      refractor_rows: Id.Map.t(int),
    )
    : flat => {
  module DeferredLinebreaks = MkDeferredLinebreaks();

  let shardify = (t: Tile.t, idx: int): Tile.t => {
    {
      ...t,
      shards: [idx],
      children: [],
    };
  };

  /* Measure a piece, recording `shape` for each row it spans */
  let calc_with_shape =
      (shape: Rows.shape, origin: Point.t, map: flat, size: Point.t) => {
    let last = Point.add(origin, size);
    let map = add_n_rows(origin, shape, size.row, map);
    (mk_measurement(origin, last), map);
  };

  /* Measure a piece that stays on its row; records no row shapes */
  let calc_inline = (origin: Point.t, map: flat, size: Point.t) => {
    let last = Point.add(origin, size);
    (mk_measurement(origin, last), map);
  };

  let add_shard = (acc: measure_acc, t: Tile.t, idx: int): measure_acc => {
    let size = Token.bounding_box(Tile.token(t, idx));
    let (measure, map) = calc_inline(acc.pos, acc.map, size);
    {
      seg: [Piece.Tile(shardify(t, idx)), ...acc.seg],
      pos: measure.last,
      map: add_s(t.id, idx, measure, map),
      row_content: update_row_content_(acc.row_content, acc.pos, size),
    };
  };

  let add_grout = (acc: measure_acc, g: Grout.t): measure_acc => {
    let size = Point.mk(~row=0, ~col=1);
    let (measure, map) = calc_inline(acc.pos, acc.map, size);
    {
      seg: [Piece.Grout(g), ...acc.seg],
      pos: measure.last,
      map: add_g(g, measure, map),
      row_content: update_row_content_(acc.row_content, acc.pos, size),
    };
  };

  let add_secondary = (acc: measure_acc, w: Secondary.t): measure_acc =>
    if (Secondary.is_linebreak(w)) {
      /* Linebreak: finish current row with its shape, start new row */
      let num_rows = DeferredLinebreaks.of_secondary();
      let row_shape = shape_of_row_content_(acc.row_content, acc.pos.col);
      let size = Point.mk(~row=num_rows, ~col=0 - acc.pos.col);
      let (measure, map) =
        calc_with_shape(row_shape, acc.pos, acc.map, size);
      let map =
        add_piece_row(
          acc.pos.row,
          acc.seg @ [Piece.Secondary(Secondary.mk_newline(Id.mk()))],
          map,
        ); /* NOTE: These linebreaks don't actually occur in the surface syntax */
      let map =
        num_rows == 0 ? map : add_n_empty_piece_rows(num_rows - 1, map);
      {
        seg: [],
        pos: measure.last,
        map: add_w(w, measure, map),
        row_content: empty_row_content_,
      };
    } else if (Secondary.is_space(w)) {
      /* Space: add to segment but don't update content bounds */
      let size = Point.mk(~row=0, ~col=Secondary.columns(w));
      let (measure, map) = calc_inline(acc.pos, acc.map, size);
      {
        seg: [Piece.Secondary(w), ...acc.seg],
        pos: measure.last,
        map: add_w(w, measure, map),
        row_content: acc.row_content,
      };
    } else {
      /* Comment or other secondary: counts as content */
      let size = Point.mk(~row=0, ~col=Secondary.columns(w));
      let (measure, map) = calc_inline(acc.pos, acc.map, size);
      {
        seg: [Piece.Secondary(w), ...acc.seg],
        pos: measure.last,
        map: add_w(w, measure, map),
        row_content: update_row_content_(acc.row_content, acc.pos, size),
      };
    };

  let add_top_level = (acc: measure_acc, ~top_level: bool): measure_acc => {
    let map =
      top_level
        ? {
          let g = DeferredLinebreaks.of_secondary();
          let row_shape = shape_of_row_content_(acc.row_content, acc.pos.col);
          add_n_rows(acc.pos, row_shape, g, acc.map)
          |> add_piece_row(
               acc.pos.row,
               acc.seg @ [Piece.Secondary(Secondary.mk_newline(Id.mk()))], /* NOTE: These linebreaks don't actually occur in the surface syntax */
               _,
             )
          |> add_n_empty_piece_rows(g - 1);
        }
        : acc.map;
    {
      ...acc,
      map,
    };
  };

  let initial_acc = {
    seg: [],
    pos: Point.zero,
    map: empty_flat,
    row_content: empty_row_content_,
  };

  /* How many splices' content [go] is inside: see the Splice case. */
  let splice_depth = ref(0);

  let rec go =
          (~top_level: bool, acc: measure_acc, seg: Segment.t): measure_acc =>
    switch (seg) {
    | [] => add_top_level(~top_level, acc)
    | [hd, ...tl] => go(~top_level, of_piece(acc, hd), tl)
    }
  and of_piece = (acc: measure_acc, p: Piece.t): measure_acc =>
    switch (p) {
    | Secondary(w) => add_secondary(acc, w)
    | Grout(g) => add_grout(acc, g)
    | Projector(p) => add_projector(acc, p)
    | Splice(s) when splice_depth^ > 0 =>
      /* A splice inside another splice's content: a livelit's cell, in
       * the pane that shows the livelit's own syntax under its GUI
       * (ProjectorPerform.ToggleSyntax), where the whole use is one
       * splice. Nothing draws it elsewhere -- the pane's editor prints
       * it inline, as its text -- so it is measured inline too. Measured
       * as zero width, every caret, click and outline after it on its
       * row landed that many columns short. Its size is still recorded,
       * for the GUI's splice_size. */
      let start = acc.pos;
      let acc = go(~top_level=false, acc, s.content);
      let last = acc.pos;
      let size =
        Point.{
          row: last.row - start.row,
          col: last.row == start.row ? last.col - start.col : last.col,
        };
      {
        ...acc,
        map: add_splice_info(s, {size: size}, acc.map),
      };
    | Splice(s) =>
      /* A Splice appearing directly in the outer segment (not inside a
       * projector) consumes zero width at its position. Its interior is
       * still measured for completeness so clicks inside the splice have
       * valid targets. */
      {
        ...acc,
        map: measure_splice(s, acc.map),
      }
    | Tile(t) =>
      /* Fold before updating the counter: a refractor's deferred rows
       * belong at the linebreak after the tile's last shard, not at any
       * linebreak inside the tile. */
      let acc =
        Aba.fold_left(
          add_shard(acc, t),
          (acc, seg) => add_shard(go(~top_level=false, acc, seg), t),
          Aba.mk(t.shards, t.children),
        );
      switch (Id.Map.find_opt(t.id, refractor_rows)) {
      | Some(n) =>
        DeferredLinebreaks.update(n) |> ignore;
        ();
      | None => ()
      };
      acc;
    }
  and add_projector = (acc: measure_acc, pr: Base.projector): measure_acc => {
    let size = DeferredLinebreaks.of_projector(pr, shape_map);
    /* Walk the projector's syntax looking for Splice children: measure
     * each splice's content in its own coordinate frame (origin = 0,0),
     * merge the resulting piece measurements back into the outer map,
     * and record the splice's intrinsic size. Non-splice pieces inside
     * the projector are not rendered inline, so they are left unmeasured. */
    let with_splices = map => measure_splices_in(pr.syntax, map);
    if (size.row == 0) {
      /* Inline projector - stays on current row */
      let (measure, map) = calc_inline(acc.pos, acc.map, size);
      {
        seg: [Piece.Projector(pr), ...acc.seg],
        pos: measure.last,
        map: add_pr(pr, measure, map) |> with_splices,
        row_content: update_row_content_(acc.row_content, acc.pos, size),
      };
    } else {
      /* Multi-line projector - finishes current row, adds new rows */
      let row_shape = shape_of_row_content_(acc.row_content, acc.pos.col);
      let (measure, map) =
        calc_with_shape(row_shape, acc.pos, acc.map, size);
      /* [calc_with_shape] records the spanned rows' max_col as the
       * projector's left edge -- right for linebreaks, whose origin is the
       * line's end, but a block projector extends [size.col] further.
       * Re-record its rows with the block's right edge so end-of-line
       * consumers (offside probe/projector views, end-of-row arms) clear
       * the block instead of anchoring at its left edge. */
      let right = acc.pos.col + size.col;
      let map =
        List.init(size.row, i => i)
        |> List.fold_left(
             (map, i) =>
               add_row(
                 acc.pos.row + i,
                 i == 0
                   ? {
                     ...row_shape,
                     content_end: max(row_shape.content_end, right),
                     max_col: right,
                   }
                   : {
                     content_start: acc.pos.col,
                     content_end: right,
                     max_col: right,
                   },
                 map,
               ),
             map,
           );
      let map =
        add_piece_row(acc.pos.row, [Piece.Projector(pr), ...acc.seg], map);
      let map = add_n_empty_piece_rows(size.row - 1, map);
      {
        seg: [],
        pos: measure.last,
        map: add_pr(pr, measure, map) |> with_splices,
        row_content: empty_row_content_,
      };
    };
  }
  /* Measure a splice's content in its own coordinate frame (origin 0,0).
   * Returns the outer map augmented with the splice's inner piece
   * measurements and the splice's intrinsic size. */
  and measure_splice = (s: Base.splice, outer: flat): flat => {
    incr(splice_depth);
    let {pos: last, map: inner, _} =
      go(~top_level=false, initial_acc, s.content);
    decr(splice_depth);
    let outer = merge_inner(inner, outer);
    /* The intrinsic size is the content's bounding box: [last] alone
     * would report the END POINT (the last line's width), understating
     * multi-line content whose longest line is not its last. The inner
     * rows map has each linebreak-terminated line's end column; the
     * final line has no linebreak, so [last.col] covers it. */
    let size =
      Point.{
        row: last.row,
        col:
          List.fold_left(
            (acc, (_, shape: Rows.shape)) => max(acc, shape.max_col),
            last.col,
            Rows.bindings(inner.rows),
          ),
      };
    add_splice_info(s, {size: size}, outer);
  }
  /* Scan a projector's syntax for Splice children and measure each.
   * Splices may sit inside tile children (e.g. a list literal whose
   * items are splices), so recurse through tiles like [splice_sizes]. */
  and measure_splices_in = (syntax: Segment.t, map: flat): flat =>
    List.fold_left(
      (map, p: Piece.t) =>
        switch (p) {
        | Splice(s) => measure_splice(s, map)
        | Projector(pr) => measure_splices_in(pr.syntax, map)
        | Tile(t) =>
          List.fold_left(
            (map, child) => measure_splices_in(child, map),
            map,
            t.children,
          )
        | Grout(_)
        | Secondary(_) => map
        },
      map,
      syntax,
    );
  go(~top_level=final, initial_acc, seg).map;
};

/* measured per chunk of whole lines (see Incr.partition) and composed
   by row offsets; lookups translate chunk rows to absolute ones */

type chunk = {
  c_anchor: Id.t, /* first piece's id: the chunk's stable identity */
  c_start: int, /* absolute starting row */
  c_height: int, /* cached flat_height: chunk_for_row runs per row_shape */
  c_pieces: Segment.t, /* the chunk's top-level pieces (for chunked views) */
  c_flat: flat,
};

type t = {
  chunks: array(chunk),
  /* piece id -> owning chunk's anchor (stable across repartitions,
     unlike indices); persistent, so old generations stay valid */
  chunk_of_id: Id.Map.t(Id.t),
  /* anchor -> index in [chunks] (rebuilt O(#chunks) per generation) */
  anchor_index: Hashtbl.t(Id.t, int),
  total_rows: int,
  /* eager: a lazy value would break structural compares of measurements */
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
  @ List.map(fst, Id.Map.bindings(f.projectors))
  @ List.map(fst, Id.Map.bindings(f.splices));

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

let find_shards_opt = (t: Tile.t, m: t): option(Shards.t) =>
  find_shards_by_id(t.id, m);

/* Like [find_by_id] but without the warning: for membership tests
 * where absence is an expected answer, not an anomaly. */
let find_by_id_quiet = (id: Id.t, m: t): option(measurement) =>
  switch (chunk_for_id(id, m)) {
  | Some(ch) =>
    find_by_id_flat(id, ch.c_flat) |> Option.map(shift_m(ch.c_start))
  | None => None
  };

/* A splice's intrinsic size, from the chunk that measured it. Splice
 * interiors are merged into their chunk in their own (splice-local)
 * frame; only the splice's size, not its position, is read from here. */
let find_splice_info_by_id = (sid: Id.t, m: t): option(splice_info) =>
  switch (chunk_for_id(sid, m)) {
  | Some(ch) => Id.Map.find_opt(sid, ch.c_flat.splices)
  | None => None
  };
let find_splice_info_opt = (s: Base.splice, m: t): option(splice_info) =>
  find_splice_info_by_id(s.id, m);
let find_splice_info = (~msg="", s: Base.splice, m: t): splice_info =>
  switch (find_splice_info_opt(s, m)) {
  | Some(info) => info
  | None => failwith("find_splice_info: " ++ msg)
  };

/* Whether the splice [sid] was measured anywhere within this map's
 * segment (including recursively inside other splices). */
let has_splice_info = (sid: Id.t, m: t): bool =>
  find_splice_info_by_id(sid, m) != None;

let row_shape = (row: int, m: t): option(Rows.shape) =>
  switch (chunk_for_row(row, m)) {
  | Some(ch) => Rows.find_opt(row - ch.c_start, ch.c_flat.rows)
  | None => None
  };

/* the row's indentation: column of its first non-whitespace */
let row_indent = (row: int, m: t): int =>
  switch (row_shape(row, m)) {
  | Some(sh) => sh.content_start
  | None => 0
  };

let min_col_of_rows = (rs: list(row), m: t): col =>
  rs
  |> List.map(r =>
       switch (row_shape(r, m)) {
       | Some(sh) => sh.content_start
       | None => Int.max_int
       }
     )
  |> List.fold_left(min, Int.max_int);

let piece_rows = (m: t): list(list(Piece.t)) => m.all_piece_rows;

let num_rows = (m: t): int => m.total_rows;

/* single-chunk measurement, without the incremental cache */
let of_segment =
    (
      ~indent_level as _: Id.Map.t(int)=Id.Map.empty,
      ~is_single_line as _: bool=false,
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorCore.Shape.t),
      refractor_rows: Id.Map.t(int),
    )
    : t => {
  let f = of_segment_inner(~final=true, seg, shape_map, refractor_rows);
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

/* Index of the last measured row (0 for empty/single-row content). */
let last_row = (m: t): int => max(0, num_rows(m) - 1);

/* Width in characters of row at measurement.origin */
let start_row_width = (measurement: measurement, measured: t): int =>
  switch (row_shape(measurement.origin.row, measured)) {
  | None => 0
  | Some(row) => row.max_col
  };

/* Compute the bounding box of a segment measured from the origin (0,0).
 * Returns a Point.t where row is the number of linebreaks and col is
 * the max column reached on the last row. Useful for computing splice
 * sizes independently of the parent segment's layout.
 *
 * [shape_map] supplies placeholder shapes for projectors inside the
 * segment; without it, nested projectors measure as if inline. */
let segment_bbox =
    (~shape_map=ProjectorCore.Shape.Map.empty, seg: Segment.t): Point.t => {
  let m = of_segment_inner(~final=true, seg, shape_map, Id.Map.empty);
  let rows = m.rows |> Rows.bindings;
  switch (rows) {
  | [] => Point.zero
  | _ =>
    /* Bounding box: the widest of ALL rows, not the last row's width —
     * a multi-line segment ending in a short line is still as wide as
     * its longest line. */
    let max_row = List.fold_left((r, (k, _)) => max(r, k), 0, rows);
    let col =
      List.fold_left(
        (acc, (_, shape: Rows.shape)) => max(acc, shape.max_col),
        0,
        rows,
      );
    {
      row: max_row,
      col,
    };
  };
};

/* Pre-pass that walks [seg] (potentially recursing into projectors and
 * splices) and returns a map from splice id to intrinsic size. This is
 * intended to be used before the full Measured map is computed, e.g.
 * when projectors need splice sizes at placeholder-time to size the
 * shape they leave for themselves in the base editor. */
let rec splice_sizes = (seg: Segment.t): Id.Map.t(Point.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Base.Splice(s) =>
        let size = segment_bbox(s.content);
        let acc = Id.Map.add(s.id, size, acc);
        Id.Map.union((_, _, v2) => Some(v2), acc, splice_sizes(s.content));
      | Base.Projector(pr) =>
        Id.Map.union((_, _, v2) => Some(v2), acc, splice_sizes(pr.syntax))
      | Base.Tile(t) =>
        List.fold_left(
          (acc, child) =>
            Id.Map.union((_, _, v2) => Some(v2), acc, splice_sizes(child)),
          acc,
          t.children,
        )
      | _ => acc
      },
    Id.Map.empty,
    seg,
  );

let splice_size_of = (sizes: Id.Map.t(Point.t), id: Id.t): Point.t =>
  switch (Id.Map.find_opt(id, sizes)) {
  | Some(p) => p
  | None => Point.zero
  };

/* incremental chunked measurement, memoized per chunk anchor: an edit
   re-measures only chunks whose pieces or shape slices changed. parity
   with the monolithic build is test-gated (Test_MeasuredChunks) */
module Incr = {
  type entry = {
    e_pieces: Segment.t,
    e_final: bool,
    e_flat: flat,
    e_ids: list(Id.t),
    /* this chunk's shape/refractor bindings, descending by id (both
       writers cons over ascending iteration); a change re-measures it */
    mutable e_shape_slice: list((Id.t, ProjectorCore.Shape.t)),
    mutable e_refr_slice: list((Id.t, int)),
  };

  /* one per editor: the last build's id->anchor map and entries; entries
     outside the current partition are dropped each build (old Measured.t
     values never consult the cache) */
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

  /* cut after the last linebreak of a secondary run when content
     follows: deferred linebreaks and the piece-row flush there, and
     nothing else carries across rows (indentation is plain whitespace),
     so a chunk measured alone matches the monolithic measurement */
  let partition = (seg: Segment.t): list((Id.t, Segment.t, bool)) =>
    switch (seg) {
    | [] => [(Id.invalid, [], true)]
    | _ =>
      let ps = Array.of_list(seg);
      let n = Array.length(ps);
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
      /* last linebreak of its secondary run, with content after? */
      let rec run_ends_here = k =>
        k >= n
          ? false
          : is_lb(ps[k])
              ? false : is_sec(ps[k]) ? run_ends_here(k + 1) : true;
      let cuts = ref([]);
      for (i in 0 to n - 1) {
        if (is_lb(ps[i]) && run_ends_here(i + 1)) {
          cuts := [i, ...cuts^];
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

  /* bindings grouped by owning anchor under [map], descending by id */
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
    /* diff the previous chunk_of_id: drop the ids of vanished or rebuilt
       chunks, then add the rebuilt ones' ids (O(changed), no dead ids) */
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
