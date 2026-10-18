open Util;
open Point;

module Point = Point;

/* tables by id, hashed by a uuid's first and last bytes: the polymorphic
   hash of an id's string is slow under jsoo. Parsed ids count in their
   first bytes and share their last; random ones vary in both */
module IdTbl =
  Hashtbl.Make({
    type t = Id.t;
    let equal = (a, b) => Id.compare(a, b) == 0;
    let hash = (id: Id.t): int => {
      let s = Uuidm.unsafe_to_bytes(id);
      let byte = i => Char.code(String.unsafe_get(s, i));
      let word = i =>
        byte(i)
        + 256
        * (byte(i + 1) + 256 * (byte(i + 2) + 256 * (byte(i + 3) land 63)));
      word(0) lxor word(12);
    };
  });

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

/* what a chunk holds, in order: whole pieces, and single shards of the
   tiles a module item splits between its head and foot (their children
   lie in the chunks between) */
type token =
  | Whole(Piece.t)
  | Shard(Tile.t, int);

let token_id = (tok: token): Id.t =>
  switch (tok) {
  | Whole(p) => Piece.id(p)
  | Shard(t, _) => t.id
  };

/* a shard is the same while its tile is, children aside: a member edit
   rebuilds the tiles around it but keeps their shells */
let token_eq = (a: token, b: token): bool =>
  switch (a, b) {
  | (Whole(p), Whole(q)) => p === q
  | (Shard(t, i), Shard(u, j)) =>
    i == j
    && (
      t === u
      || t.id == u.id
      && t.form == u.form
      && t.sort == u.sort
      && t.shards == u.shards
    )
  | _ => false
  };

let rec tokens_eq = (a: list(token), b: list(token)): bool =>
  a === b
  || (
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) => token_eq(x, y) && tokens_eq(xs, ys)
    | _ => false
    }
  );

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

// returns the measurement spanning the whole tile
let tile_span = (t: Tile.t, shards: Shards.t): measurement => {
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

let of_tokens_inner =
    (
      ~final: bool,
      tokens: list(token),
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

  let add_projector = (acc: measure_acc, pr: Base.projector): measure_acc => {
    let size = DeferredLinebreaks.of_projector(pr, shape_map);
    if (size.row == 0) {
      /* Inline projector - stays on current row */
      let (measure, map) = calc_inline(acc.pos, acc.map, size);
      {
        seg: [Piece.Projector(pr), ...acc.seg],
        pos: measure.last,
        map: add_pr(pr, measure, map),
        row_content: update_row_content_(acc.row_content, acc.pos, size),
      };
    } else {
      /* Multi-line projector - finishes current row, adds new rows */
      let row_shape = shape_of_row_content_(acc.row_content, acc.pos.col);
      let (measure, map) =
        calc_with_shape(row_shape, acc.pos, acc.map, size);
      let map =
        add_piece_row(acc.pos.row, [Piece.Projector(pr), ...acc.seg], map);
      let map = add_n_empty_piece_rows(size.row - 1, map);
      {
        seg: [],
        pos: measure.last,
        map: add_pr(pr, measure, map),
        row_content: empty_row_content_,
      };
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

  /* a refractor's deferred rows belong at the linebreak after its tile's
     last shard, not at any linebreak inside the tile */
  let defer_rows = (t: Tile.t) =>
    switch (Id.Map.find_opt(t.id, refractor_rows)) {
    | Some(n) => DeferredLinebreaks.update(n)
    | None => ()
    };

  let rec go = (acc: measure_acc, seg: Segment.t): measure_acc =>
    List.fold_left(of_piece, acc, seg)
  and of_piece = (acc: measure_acc, p: Piece.t): measure_acc =>
    switch (p) {
    | Secondary(w) => add_secondary(acc, w)
    | Grout(g) => add_grout(acc, g)
    | Projector(p) => add_projector(acc, p)
    | Tile(t) =>
      let acc =
        Aba.fold_left(
          add_shard(acc, t),
          (acc, seg) => add_shard(go(acc, seg), t),
          Aba.mk(t.shards, t.children),
        );
      defer_rows(t);
      acc;
    };
  let of_token = (acc: measure_acc, tok: token): measure_acc =>
    switch (tok) {
    | Whole(p) => of_piece(acc, p)
    | Shard(t, i) =>
      let acc = add_shard(acc, t, i);
      if (i == Tile.r_shard(t)) {
        defer_rows(t);
      };
      acc;
    };
  let initial_acc = {
    seg: [],
    pos: Point.zero,
    map: empty_flat,
    row_content: empty_row_content_,
  };
  let acc = List.fold_left(of_token, initial_acc, tokens);
  add_top_level(~top_level=final, acc).map;
};

let of_segment_inner =
    (
      ~final: bool,
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorCore.Shape.t),
      refractor_rows: Id.Map.t(int),
    )
    : flat =>
  of_tokens_inner(
    ~final,
    List.map(p => Whole(p), seg),
    shape_map,
    refractor_rows,
  );

/* measured per chunk of whole lines (see Partition) and composed by row
   offsets; lookups translate chunk rows to absolute ones */

type chunk = {
  /* its item's anchor (ItemSpans), derived for a module's head and foot:
     the chunk's stable identity */
  c_anchor: Id.t,
  c_start: int, /* absolute starting row */
  c_height: int, /* cached flat_height: chunk_for_row runs per row_shape */
  c_tokens: list(token), /* the chunk's contents (for chunked views) */
  c_flat: flat,
  /* inside a module body: it holds no tile of the program's top level */
  c_nested: bool,
};

type t = {
  chunks: array(chunk),
  /* piece id -> owning chunk's anchor (stable across repartitions,
     unlike indices); persistent, so old generations stay valid. A split
     tile maps to its foot, which applies its refractor rows */
  chunk_of_id: Id.Map.t(Id.t),
  /* a tile split between a module's head and foot -> (head, foot) */
  split: Id.Map.t((Id.t, Id.t)),
  /* anchor -> index in [chunks] (rebuilt O(#chunks) per generation) */
  anchor_index: IdTbl.t(int),
  total_rows: int,
  /* every row's pieces, composed on first use: a cache, not a lazy value
     (a closure would break structural compares of measurements) */
  mutable all_piece_rows: option(list(list(Piece.t))),
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

/* [flats]: each chunk's anchor, tokens, measurement, height and whether
   it is nested in a module body */
let mk_chunked =
    (
      ~chunk_of_id,
      ~split,
      flats: list((Id.t, list(token), flat, int, bool)),
    )
    : t => {
  let n = List.length(flats);
  let anchor_index = IdTbl.create(n > 0 ? n : 1);
  let (chunks_rev, total, _) =
    List.fold_left(
      ((acc, row, i), (anchor, tokens, f, h, nested)) => {
        IdTbl.replace(anchor_index, anchor, i);
        (
          [
            {
              c_anchor: anchor,
              c_start: row,
              c_height: h,
              c_tokens: tokens,
              c_flat: f,
              c_nested: nested,
            },
            ...acc,
          ],
          row + h,
          i + 1,
        );
      },
      ([], 0, 0),
      flats,
    );
  let chunks = Array.of_list(List.rev(chunks_rev));
  {
    chunks,
    chunk_of_id,
    split,
    anchor_index,
    total_rows: total,
    all_piece_rows: None,
  };
};

let chunk_of_anchor = (anchor: Id.t, m: t): option(chunk) =>
  switch (IdTbl.find_opt(m.anchor_index, anchor)) {
  | Some(i) => Some(m.chunks[i])
  | None => None
  };

let chunk_for_id = (id: Id.t, m: t): option(chunk) =>
  switch (Id.Map.find_opt(id, m.chunk_of_id)) {
  | None => None
  | Some(anchor) => chunk_of_anchor(anchor, m)
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

let shift_shards = (s: int, shards: Shards.t): Shards.t =>
  List.map(((i, meas)) => (i, shift_m(s, meas)), shards);

/* [id]'s binding in one of its chunk's maps, in absolute rows */
let find_in =
    (field: flat => Id.Map.t('a), shift: (int, 'a) => 'b, id: Id.t, m: t)
    : option('b) =>
  switch (chunk_for_id(id, m)) {
  | Some(ch) =>
    Id.Map.find_opt(id, field(ch.c_flat)) |> Option.map(shift(ch.c_start))
  | None => None
  };

let or_fail = (what: string, msg: string, found: option('a)): 'a =>
  switch (found) {
  | Some(x) => x
  | None => failwith(what ++ ": " ++ msg)
  };

/* a tile's shards; a split tile's from its head and its foot, whose
   shards all follow the head's */
let find_shards_by_id = (id: Id.t, m: t): option(Shards.t) =>
  switch (Id.Map.find_opt(id, m.split)) {
  | Some((head, foot)) =>
    let part = anchor =>
      switch (chunk_of_anchor(anchor, m)) {
      | Some(ch) =>
        switch (Id.Map.find_opt(id, ch.c_flat.tiles)) {
        | Some(shards) => shift_shards(ch.c_start, shards)
        | None => []
        }
      | None => []
      };
    switch (part(head) @ part(foot)) {
    | [] => None
    | shards => Some(shards)
    };
  | None => find_in(f => f.tiles, shift_shards, id, m)
  };
let find_shards = (~msg="", t: Tile.t, m: t): Shards.t =>
  or_fail("find_shards", msg, find_shards_by_id(t.id, m));
let find_w = (~msg="", w: Secondary.t, m: t): measurement =>
  or_fail("find_w", msg, find_in(f => f.secondary, shift_m, w.id, m));
let find_g = (~msg="", g: Grout.t, m: t): measurement =>
  or_fail("find_g", msg, find_in(f => f.grout, shift_m, g.id, m));
let find_pr_opt = (p: Base.projector, m: t): option(measurement) =>
  find_in(f => f.projectors, shift_m, p.id, m);
let find_pr = (~msg="", p: Base.projector, m: t): measurement =>
  or_fail("find_pr", msg, find_pr_opt(p, m));
let find_p = (~msg="", p: Piece.t, m: t): measurement =>
  try(
    p
    |> Piece.get(
         w => find_w(w, m),
         g => find_g(g, m),
         t =>
           find_shards_by_id(t.id, m)
           |> Option.map(tile_span(t))
           |> or_fail("find_t", msg),
         p => find_pr(p, m),
       )
  ) {
  | _ => failwith("find_p: " ++ msg ++ "id: " ++ Id.to_string(p |> Piece.id))
  };
let find_by_id = (id: Id.t, m: t): option(measurement) =>
  switch (Id.Map.mem(id, m.split), chunk_for_id(id, m)) {
  | (true, _) =>
    find_shards_by_id(id, m)
    |> Option.map(shards =>
         {
           origin: snd(List.hd(shards)).origin,
           last: snd(ListUtil.last(shards)).last,
         }
       )
  | (false, Some(ch)) =>
    find_by_id_flat(id, ch.c_flat) |> Option.map(shift_m(ch.c_start))
  | (false, None) =>
    Printf.printf("Measured.WARNING: id %s not found", Id.to_string(id));
    None;
  };

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

let piece_rows = (m: t): list(list(Piece.t)) =>
  switch (m.all_piece_rows) {
  | Some(rows) => rows
  | None =>
    let rows =
      Array.fold_left((acc, ch) => ch.c_flat.piece_rows @ acc, [], m.chunks);
    m.all_piece_rows = Some(rows);
    rows;
  };

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
  let tokens = List.map(p => Whole(p), seg);
  let f = of_tokens_inner(~final=true, tokens, shape_map, refractor_rows);
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
  mk_chunked(
    ~chunk_of_id,
    ~split=Id.Map.empty,
    [(anchor, tokens, f, flat_height(f), false)],
  );
};

/* Width in characters of row at measurement.origin */
let start_row_width = (measurement: measurement, measured: t): int =>
  switch (row_shape(measurement.origin.row, measured)) {
  | None => 0
  | Some(row) => row.max_col
  };

/* the layout's chunks (plans/hierarchical-spans.md §4.3): items' line
   slots (ItemSpans), merged where items share a line, with the trivia
   between slots a chunk of its own. A module literal whose members start
   on their own lines splits into a head, its members' chunks and a foot;
   a member that is a let chain, into its items' chunks. Every cut follows
   a linebreak: deferred linebreaks and the piece-row flush there, and
   nothing else carries across rows (indentation is plain whitespace), so a
   chunk measured alone matches the monolithic measurement */
module Partition = {
  module S = ItemSpans;

  type part = {
    p_anchor: Id.t,
    p_tokens: list(token),
    /* cut inside a module body */
    p_nested: bool,
    p_final: bool,
  };

  /* a module body's chunks between its head and foot (in order), kept
     while ItemSpans keeps its level: an edit redoes only the bodies it
     changed */
  type body = {
    b_level: S.level,
    b_edges: option((int, int)),
    b_parts: list(part),
    b_splits: list((Id.t, (Id.t, Id.t))),
  };

  /* one per editor: the last partition's bodies by brace id, and the
     current one's */
  type memo = {
    mutable last: IdTbl.t(body),
    mutable this: IdTbl.t(body),
  };
  let mk_memo = (): memo => {
    last: IdTbl.create(8),
    this: IdTbl.create(8),
  };

  /* the chunks so far: the open one's tokens (reversed) and anchor once
     known, the closed ones, and the split tiles with (head, foot) */
  type builder = {
    memo,
    nested: bool,
    mutable open_: list(token),
    mutable name: option(Id.t),
    mutable parts: list(part),
    mutable splits: list((Id.t, (Id.t, Id.t))),
  };

  /* a module item's head and foot anchors by its anchor: Id.derive
     hashes, and every partition names every split item */
  let names: Swept.t(Id.t, (Id.t, Id.t)) = Swept.mk();
  let names_of = (anchor: Id.t): (Id.t, Id.t) =>
    switch (Swept.find_opt(names, anchor)) {
    | Some(hf) => hf
    | None =>
      let hf = (
        Id.derive(~salt="head", anchor),
        Id.derive(~salt="foot", anchor),
      );
      Swept.replace(names, anchor, hf);
      hf;
    };

  /* test hook: bodies partitioned, cumulative */
  let bodies_built = ref(0);

  let builder = (~nested, memo: memo): builder => {
    memo,
    nested,
    open_: [],
    name: None,
    parts: [],
    splits: [],
  };

  let push = (b: builder, tok: token) => b.open_ = [tok, ...b.open_];
  let push_range = (b: builder, arr: array(Piece.t), lo: int, hi: int) =>
    for (i in lo to hi - 1) {
      push(b, Whole(arr[i]));
    };
  /* a chunk is named by the first item starting in it, else by its first
     piece */
  let name = (b: builder, id: Id.t) =>
    if (b.name == None) {
      b.name = Some(id);
    };
  let close = (b: builder) =>
    switch (List.rev(b.open_)) {
    | [] => ()
    | [first, ..._] as toks =>
      let anchor = Option.value(b.name, ~default=token_id(first));
      b.parts = [
        {
          p_anchor: anchor,
          p_tokens: toks,
          p_nested: b.nested,
          p_final: false,
        },
        ...b.parts,
      ];
      b.open_ = [];
      b.name = None;
    };

  /* [lv]'s cut points in (a, b): its items' line slot edges that follow a
     linebreak, ascending. A slot can end at its level's end without one:
     a let chain's, before its member's `;` */
  let cuts = (lv: S.level, a: int, b: int): list(int) => {
    let ok = (x, acc) =>
      a < x
      && x < b
      && Piece.is_linebreak(lv.pieces[x - 1])
      && (
        switch (acc) {
        | [y, ..._] => y != x
        | [] => true
        }
      );
    Array.fold_right(
      (sp: S.span, acc) =>
        switch (sp.lines) {
        | Some((lo, hi)) =>
          let acc = ok(hi, acc) ? [hi, ...acc] : acc;
          ok(lo, acc) ? [lo, ...acc] : acc;
        | None => acc
        },
      lv.spans,
      [],
    );
  };

  /* a module literal: its definition's site, its brace and where the
     brace sits there, its members, and their first and last cuts */
  type literal = {
    site: S.site,
    at: int,
    brace: Tile.t,
    body,
    first: int,
    last: int,
  };

  type split =
    | Module(literal)
    /* a member that is a let chain: its items */
    | Chain(S.level);

  /* where a module body's head ends and its foot starts: after the `{`
     line and at the `}` line, unless a member shares the line; then at
     the members' first or last cut */
  let edges = (ml: S.level): option((int, int)) => {
    let arr = ml.pieces;
    let len = Array.length(arr);
    let rec trivia = (i, z) =>
      i >= z || Piece.is_secondary(arr[i]) && trivia(i + 1, z);
    let rec fwd = i =>
      i < len && !Piece.is_linebreak(arr[i]) ? fwd(i + 1) : i;
    let rec back = i =>
      i >= 0 && !Piece.is_linebreak(arr[i]) ? back(i - 1) : i;
    let cs = cuts(ml, 0, len + 1);
    let (lb0, lbn) = (fwd(0), back(len - 1));
    let first =
      lb0 < len && trivia(0, lb0) ? Some(lb0 + 1) : ListUtil.hd_opt(cs);
    let last =
      lbn >= 0 && trivia(lbn + 1, len)
        ? Some(lbn + 1) : ListUtil.last_opt(cs);
    switch (first, last) {
    | (Some(f), Some(l)) when f < l => Some((f, l))
    | _ => None
    };
  };

  /* [lv]'s pieces [a, z), cut at its cut points between; the caller
     closes at [z] */
  let rec level = (b: builder, lv: S.level, a: int, z: int) => {
    let n = Array.length(lv.spans);
    /* where item [k] starts: its first content (a core can open with a
       comment its slot leaves out) */
    let starts = k => {
      let (c0, c1) = lv.spans[k].core;
      let rec go = i =>
        i < c1 && Piece.is_secondary(lv.pieces[i]) ? go(i + 1) : i;
      go(c0);
    };
    let rec from = k => k < n && starts(k) < a ? from(k + 1) : k;
    /* the items starting before [x], from [k] */
    let rec upto = (k, x, acc) =>
      k < n && starts(k) < x
        ? upto(k + 1, x, [k, ...acc]) : (k, List.rev(acc));
    let rec regions = (r0, pts, k) =>
      switch (pts) {
      | [] => ()
      | [r1, ...pts] =>
        let (k', ks) = upto(k, r1, []);
        region(b, lv, r0, r1, ks);
        if (pts != []) {
          close(b);
        };
        regions(r1, pts, k');
      };
    regions(a, cuts(lv, a, z) @ [z], from(0));
  }
  /* how item [k] splits, if its children start lines of their own */
  and split_of = (memo: memo, lv: S.level, k: int): option(split) =>
    switch (S.members(lv, k)) {
    | Some(members) =>
      switch (S.site_of(lv, k)) {
      | Some(site) =>
        switch (S.brace_of(lv.pieces, site)) {
        | Some((at, brace)) =>
          let body = body_of(memo, brace.id, members);
          Option.map(
            ((first, last)) =>
              Module({
                site,
                at,
                brace,
                body,
                first,
                last,
              }),
            body.b_edges,
          );
        | None => None
        }
      | None => None
      }
    | None =>
      switch (S.chain(lv, k)) {
      | Some(cl) when cuts(cl, 0, Array.length(cl.pieces)) != [] =>
        Some(Chain(cl))
      | _ => None
      }
    }
  /* a module body's chunks, from the memo while its level is the same */
  and body_of = (memo: memo, id: Id.t, ml: S.level): body => {
    let body =
      switch (IdTbl.find_opt(memo.last, id)) {
      | Some(body) when body.b_level === ml => body
      | _ =>
        incr(bodies_built);
        let edges = edges(ml);
        let sub = builder(~nested=true, memo);
        Option.iter(
          ((first, last)) => {
            level(sub, ml, first, last);
            close(sub);
          },
          edges,
        );
        {
          b_level: ml,
          b_edges: edges,
          b_parts: List.rev(sub.parts),
          b_splits: sub.splits,
        };
      };
    IdTbl.replace(memo.this, id, body);
    body;
  }
  /* pieces [r0, r1) of [lv], holding items [ks]: a lone item splits if
     it can */
  and region = (b: builder, lv: S.level, r0: int, r1: int, ks: list(int)) =>
    switch (ks) {
    | [k] =>
      switch (split_of(b.memo, lv, k)) {
      | Some(Module(lit)) => literal(b, lv, k, r0, r1, lit)
      | Some(Chain(cl)) =>
        push_range(b, lv.pieces, r0, cl.offset);
        level(b, cl, 0, Array.length(cl.pieces));
        push_range(b, lv.pieces, cl.offset + Array.length(cl.pieces), r1);
      | None =>
        name(b, lv.spans[k].anchor);
        push_range(b, lv.pieces, r0, r1);
      }
    | [k, ..._] =>
      name(b, lv.spans[k].anchor);
      push_range(b, lv.pieces, r0, r1);
    | [] => push_range(b, lv.pieces, r0, r1)
    }
  /* the head runs through the brace's `{` to the first member cut, and the
     foot from the last member cut through the `}` and the item's end; the
     tiles around the members are split between them */
  and literal =
      (b: builder, lv: S.level, k: int, r0: int, r1: int, lit: literal) => {
    let (head, foot) = names_of(lv.spans[k].anchor);
    b.name = Some(head);
    switch (lit.site) {
    | Run(_) =>
      push_range(b, lv.pieces, r0, lit.at);
      push(b, Shard(lit.brace, 0));
    | Kid(h, t) =>
      push_range(b, lv.pieces, r0, h);
      let last_kid = List.length(t.children) - 1;
      List.iteri(
        (j, kid) => {
          push(b, Shard(t, j));
          if (j < last_kid) {
            List.iter(p => push(b, Whole(p)), kid);
          } else {
            push_range(b, Array.of_list(kid), 0, lit.at);
            push(b, Shard(lit.brace, 0));
          };
        },
        t.children,
      );
      b.splits = [(t.id, (head, foot)), ...b.splits];
    };
    let members = lit.body.b_level.pieces;
    push_range(b, members, 0, lit.first);
    close(b);
    b.parts = List.rev_append(lit.body.b_parts, b.parts);
    b.splits = List.rev_append(lit.body.b_splits, b.splits);
    b.name = Some(foot);
    push_range(b, members, lit.last, Array.length(members));
    push(b, Shard(lit.brace, 1));
    switch (lit.site) {
    | Run(_) => push_range(b, lv.pieces, lit.at + 1, r1)
    | Kid(h, t) =>
      let kid = Array.of_list(ListUtil.last(t.children));
      push_range(b, kid, lit.at + 1, Array.length(kid));
      push(b, Shard(t, List.length(t.children)));
      push_range(b, lv.pieces, h + 1, r1);
    };
    b.splits = [(lit.brace.id, (head, foot)), ...b.splits];
  };

  /* the program's chunks in order, the last final; and its split tiles */
  let go =
      (~root: Sort.t, ~memo: memo, seg: Segment.t)
      : (list(part), list((Id.t, (Id.t, Id.t)))) =>
    switch (seg) {
    | [] => (
        [
          {
            p_anchor: Id.invalid,
            p_tokens: [],
            p_nested: false,
            p_final: true,
          },
        ],
        [],
      )
    | _ =>
      let top = S.top(~root, seg);
      Swept.next_pass(names);
      let b = builder(~nested=false, memo);
      level(b, top, 0, Array.length(top.pieces));
      close(b);
      /* keep the bodies this partition reached */
      let reached = memo.this;
      IdTbl.reset(memo.last);
      memo.this = memo.last;
      memo.last = reached;
      let parts =
        switch (b.parts) {
        | [last, ...parts] => [
            {
              ...last,
              p_final: true,
            },
            ...parts,
          ]
        | [] => []
        };
      (List.rev(parts), b.splits);
    };
};

/* incremental chunked measurement, memoized per chunk anchor: an edit
   re-measures only chunks whose tokens or shape slices changed. parity
   with the monolithic build is test-gated (Test_MeasuredChunks) */
module Incr = {
  type entry = {
    e_tokens: list(token),
    e_final: bool,
    e_flat: flat,
    e_height: int,
    e_ids: list(Id.t),
    /* this chunk's shape/refractor bindings, descending by id (both
       writers cons over ascending iteration); a change re-measures it */
    mutable e_shape_slice: list((Id.t, ProjectorCore.Shape.t)),
    mutable e_refr_slice: list((Id.t, int)),
    /* the last build that used it */
    mutable e_gen: int,
  };

  /* one per editor: the last build's id->anchor map and entries; entries
     outside the current partition are dropped each build (old Measured.t
     values never consult the cache) */
  type cache = {
    mutable gen: int,
    mutable prev: option((Id.Map.t(Id.t), IdTbl.t(entry))),
    bodies: Partition.memo,
    /* the last split tiles and their map */
    mutable split: (list((Id.t, (Id.t, Id.t))), Id.Map.t((Id.t, Id.t))),
  };
  let mk_cache = (): cache => {
    gen: 0,
    prev: None,
    bodies: Partition.mk_memo(),
    split: ([], Id.Map.empty),
  };

  /* test hook: chunks re-measured, cumulative */
  let built = ref(0);

  /* bindings grouped by owning anchor under [map], descending by id */
  let slices_of =
      (map: Id.Map.t(Id.t), bindings: list((Id.t, 'a)))
      : IdTbl.t(list((Id.t, 'a))) => {
    let h = IdTbl.create(8);
    List.iter(
      ((id, v)) =>
        switch (Id.Map.find_opt(id, map)) {
        | Some(anchor) =>
          let cur =
            switch (IdTbl.find_opt(h, anchor)) {
            | Some(l) => l
            | None => []
            };
          IdTbl.replace(h, anchor, [(id, v), ...cur]);
        | None => ()
        },
      bindings,
    );
    h;
  };

  /* most chunks hold no projector or drawer */
  let slice_for = (h: IdTbl.t(list('a)), anchor: Id.t): list('a) =>
    IdTbl.length(h) == 0
      ? []
      : (
        switch (IdTbl.find_opt(h, anchor)) {
        | Some(l) => l
        | None => []
        }
      );

  let of_segment =
      (
        ~root: Sort.t=Exp,
        ~cache: cache,
        seg: Segment.t,
        shape_map: Id.Map.t(ProjectorCore.Shape.t),
        refractor_shape_map: Id.Map.t(int),
      )
      : t => {
    let (parts, splits) = Partition.go(~root, ~memo=cache.bodies, seg);
    let (prev_map, prev_entries) =
      switch (cache.prev) {
      | Some((m, e)) => (m, e)
      | None => (Id.Map.empty, IdTbl.create(1))
      };
    cache.gen = cache.gen + 1;
    let gen = cache.gen;
    let shape_bindings = Id.Map.bindings(shape_map);
    let refr_bindings = Id.Map.bindings(refractor_shape_map);
    let shape_slices = slices_of(prev_map, shape_bindings);
    let refr_slices = slices_of(prev_map, refr_bindings);
    let new_entries = IdTbl.create(List.length(parts));
    let chunks =
      List.map(
        (
          {
            p_anchor: anchor,
            p_tokens: tokens,
            p_final: final,
            p_nested: nested,
          }: Partition.part,
        ) => {
          let (e, carried) =
            switch (IdTbl.find_opt(prev_entries, anchor)) {
            | Some(e)
                when
                  e.e_final == final
                  && tokens_eq(e.e_tokens, tokens)
                  && e.e_shape_slice == slice_for(shape_slices, anchor)
                  && e.e_refr_slice == slice_for(refr_slices, anchor) => (
                e,
                true,
              )
            | _ =>
              incr(built);
              let f =
                of_tokens_inner(
                  ~final,
                  tokens,
                  shape_map,
                  refractor_shape_map,
                );
              (
                {
                  e_tokens: tokens,
                  e_final: final,
                  e_flat: f,
                  e_height: flat_height(f),
                  e_ids: ids_of_flat(f),
                  e_shape_slice: [], /* filled below from the new map */
                  e_refr_slice: [],
                  e_gen: gen,
                },
                false,
              );
            };
          e.e_gen = gen;
          IdTbl.replace(new_entries, anchor, e);
          (anchor, tokens, e, carried, nested);
        },
        parts,
      );
    /* diff the previous chunk_of_id: drop the ids of the entries this
       build didn't reach (vanished or rebuilt chunks), then add the
       rebuilt ones' ids (O(changed), no dead ids). A chunk rebuilt under
       its old anchor keeps most of its ids, which stay put */
    let map = ref(prev_map);
    let rebuilt = IdTbl.create(64);
    List.iter(
      ((anchor, _, e: entry, carried, _)) =>
        if (!carried) {
          List.iter(id => IdTbl.replace(rebuilt, id, anchor), e.e_ids);
        },
      chunks,
    );
    IdTbl.iter(
      (anchor, old_e: entry) =>
        if (old_e.e_gen != gen) {
          List.iter(
            id =>
              switch (IdTbl.find_opt(rebuilt, id)) {
              | Some(a) when a === anchor => ()
              | _ => map := Id.Map.remove(id, map^)
              },
            old_e.e_ids,
          );
        },
      prev_entries,
    );
    IdTbl.iter(
      (id, anchor) =>
        switch (Id.Map.find_opt(id, map^)) {
        | Some(a) when a === anchor => ()
        | _ => map := Id.Map.add(id, anchor, map^)
        },
      rebuilt,
    );
    /* a split tile is in two chunks: it maps to its foot */
    List.iter(
      ((id, (_, foot))) =>
        switch (Id.Map.find_opt(id, map^)) {
        | Some(a) when a === foot => ()
        | _ => map := Id.Map.add(id, foot, map^)
        },
      splits,
    );
    let chunk_of_id = map^;
    let shape_slices = slices_of(chunk_of_id, shape_bindings);
    let refr_slices = slices_of(chunk_of_id, refr_bindings);
    List.iter(
      ((anchor, _, e: entry, _, _)) => {
        e.e_shape_slice = slice_for(shape_slices, anchor);
        e.e_refr_slice = slice_for(refr_slices, anchor);
      },
      chunks,
    );
    cache.prev = Some((chunk_of_id, new_entries));
    /* the same splits hold the same ids and named pairs */
    let rec same = (a, b) =>
      switch (a, b) {
      | ([], []) => true
      | ([(i, hf), ...a], [(j, hf'), ...b]) =>
        i === j && hf === hf' && same(a, b)
      | _ => false
      };
    if (!same(fst(cache.split), splits)) {
      cache.split = (splits, Id.Map.of_list(splits));
    };
    mk_chunked(
      ~chunk_of_id,
      ~split=snd(cache.split),
      List.map(
        ((a, tokens, e: entry, _, nested)) =>
          (a, tokens, e.e_flat, e.e_height, nested),
        chunks,
      ),
    );
  };
};
