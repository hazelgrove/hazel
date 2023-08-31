open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type row = int;
[@deriving (show({with_path: false}), sexp, yojson)]
type col = int;

module Point = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    row,
    col,
  };
  let zero = {row: 0, col: 0};

  let equals: (t, t) => bool = (p, q) => p.row == q.row && p.col == q.col;

  type comparison =
    | Exact
    | Under
    | Over;

  let comp = (current, target): comparison =>
    switch () {
    | _ when current == target => Exact
    | _ when current < target => Under
    | _ => Over
    };
  let compare = (p1, p2) =>
    switch (comp(p1, p2)) {
    | Exact => 0
    | Under => (-1)
    | Over => 1
    };

  let dcomp = (direction: Direction.t, a, b) =>
    switch (direction) {
    | Right => comp(a, b)
    | Left => comp(b, a)
    };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type measurement = {
  origin: Point.t,
  last: Point.t,
};

// indentation relative to container
type rel_indent = int;
// indentation relative to code container
type abs_indent = int;

module Rows = {
  include IntMap;
  type shape = {
    indent: col,
    max_col: col,
  };
  type t = IntMap.t(shape);

  let max_col = (rs: list(row), map: t) =>
    rs |> List.map(r => find(r, map).max_col) |> List.fold_left(max, 0);

  let min_col = (rs: list(row), map: t) =>
    rs
    |> List.map(r => find(r, map).indent)
    |> List.fold_left(min, Int.max_int);
};

module Shards = {
  type shard = (int, measurement);
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

type t = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  secondary: Id.Map.t(measurement),
  rows: Rows.t,
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  secondary: Id.Map.empty,
  rows: Rows.empty,
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

// assumes tile is single shard
let add_t = (t: Tile.t, m, map) => {
  ...map,
  tiles:
    map.tiles
    |> Id.Map.update(
         t.id,
         fun
         | None => Some([(Tile.l_shard(t), m)])
         | Some(ms) => Some([(Tile.l_shard(t), m), ...ms]),
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
let add_p = (p: Piece.t, m, map) =>
  p
  |> Piece.get(
       w => add_w(w, m, map),
       g => add_g(g, m, map),
       t => add_t(t, m, map),
     );

let add_row = (row: int, shape: Rows.shape, map) => {
  ...map,
  rows: Rows.add(row, shape, map.rows),
};

let find_shards = (t: Tile.t, map) => Id.Map.find(t.id, map.tiles);

let find_w = (w: Secondary.t, map): measurement =>
  Id.Map.find(w.id, map.secondary);
let find_g = (g: Grout.t, map): measurement => Id.Map.find(g.id, map.grout);
// returns the measurement spanning the whole tile
let find_t = (t: Tile.t, map): measurement => {
  let shards = find_shards(t, map);
  let first = ListUtil.assoc_err(Tile.l_shard(t), shards, "find_t");
  let last = ListUtil.assoc_err(Tile.r_shard(t), shards, "find_t");
  {origin: first.origin, last: last.last};
};

let find_p = (p: Piece.t, map): measurement =>
  p
  |> Piece.get(
       w => find_w(w, map),
       g => find_g(g, map),
       t => find_t(t, map),
     );

let find_by_id = (id: Id.t, map: t): option(measurement) => {
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
        Some({origin: first.origin, last: last.last});
      | None =>
        Printf.printf("Measured.WARNING: id %s not found", Id.to_string(id));
        None;
      }
    }
  };
};

let of_segment = (~indent_level=Id.Map.empty, seg: Segment.t): t => {
  let indent_level =
    Id.Map.is_empty(indent_level)
      ? Indentation.level_map(seg) : indent_level;
  let rec go_nested =
          (~map, ~prev_indent, ~origin, seg: Segment.t): (Point.t, t) => {
    let rec go_seq =
            (~map, ~prev_indent: int, ~origin: Point.t, seg: Segment.t)
            : (int, Point.t, t) =>
      switch (seg) {
      | [] =>
        let map =
          map
          |> add_row(origin.row, {indent: prev_indent, max_col: origin.col});
        (prev_indent, origin, map);
      | [hd, ...tl] =>
        let (prev_indent, origin, map) =
          switch (hd) {
          | Secondary(w) when Secondary.is_linebreak(w) =>
            let row_shape = Rows.{indent: prev_indent, max_col: origin.col};
            let indent =
              switch (Id.Map.find_opt(w.id, indent_level)) {
              | Some(indent) => indent
              | None => 0
              };
            let last = Point.{row: origin.row + 1, col: indent};
            let map =
              map
              |> add_w(w, {origin, last})
              |> add_row(origin.row, row_shape);
            (indent, last, map);
          | Secondary(w) =>
            let last = {...origin, col: origin.col + Secondary.length(w)};
            let map = add_w(w, {origin, last}, map);
            (prev_indent, last, map);
          | Grout(g) =>
            let last = {...origin, col: origin.col + 1};
            let map = add_g(g, {origin, last}, map);
            (prev_indent, last, map);
          | Tile(t) =>
            let token = List.nth(t.label);
            let add_shard = (origin, map, shard) => {
              let last =
                Point.{
                  ...origin,
                  col: origin.col + String.length(token(shard)),
                };
              let map = add_s(t.id, shard, {origin, last}, map);
              (last, map);
            };
            let (last, map) =
              Aba.mk(t.shards, t.children)
              |> Aba.fold_left(
                   add_shard(origin, map),
                   ((origin, map), child, shard) => {
                     let (child_last, child_map) =
                       go_nested(~map, ~prev_indent, ~origin, child);
                     add_shard(child_last, child_map, shard);
                   },
                 );
            (prev_indent, last, map);
          };
        go_seq(~map, ~prev_indent, ~origin, tl);
      };
    let (_, tl_last, map) = go_seq(~map, ~origin, ~prev_indent, seg);
    (tl_last, map);
  };
  snd(go_nested(~map=empty, ~prev_indent=0, ~origin=Point.zero, seg));
};

let segment_origin = (seg: Segment.t): option(Point.t) =>
  Option.map(
    first => find_p(first, of_segment(seg)).origin,
    ListUtil.hd_opt(seg),
  );

let segment_last = (seg: Segment.t): option(Point.t) =>
  Option.map(
    last => find_p(last, of_segment(seg)).last,
    ListUtil.last_opt(seg),
  );

let segment_height = (seg: Segment.t) =>
  switch (segment_last(seg), segment_origin(seg)) {
  | (Some(last), Some(first)) => 1 + last.row - first.row
  | _ => 0
  };

let segment_width = (seg: Segment.t): int =>
  IntMap.fold(
    (_, {max_col, _}: Rows.shape, acc) => max(max_col, acc),
    of_segment(seg).rows,
    0,
  );
