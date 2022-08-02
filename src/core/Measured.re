open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type point = {
  // indent: int,
  row: int,
  col: int,
};
let zero = {row: 0, col: 0};

[@deriving (show({with_path: false}), sexp, yojson)]
type measurement_lin = {
  origin: int,
  length: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type measurement = {
  origin: point,
  last: point,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type token = {
  row: int,
  indent: int,
  range: (int, int),
};

module Rows = {
  include IntMap;
  type shape = {
    indent: int,
    max_col: int,
  };
  type t = IntMap.t(shape);

  let max_col = (rs: list(int), map: t) =>
    rs |> List.map(r => find(r, map).max_col) |> List.fold_left(max, 0);
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
  // let last = (shards: t) =>
  //   shards
  //   |> List.sort(((i, _), (j, _)) => Int.compare(i, j))
  //   |> ListUtil.last_opt
  //   |> Option.map(snd);
};

type t = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  whitespace: Id.Map.t(measurement),
  rows: Rows.t,
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  whitespace: Id.Map.empty,
  rows: Rows.empty,
};

let point_equals: (point, point) => bool =
  (p, q) => p.row == q.row && p.col == q.col;

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
let add_w = (w: Whitespace.t, m, map) => {
  ...map,
  whitespace: map.whitespace |> Id.Map.add(w.id, m),
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

let singleton_w = (w, m) => empty |> add_w(w, m);
let singleton_g = (g, m) => empty |> add_g(g, m);
let singleton_s = (id, shard, m) => empty |> add_s(id, shard, m);

// TODO(d) rename
let find_opt_shards = (t: Tile.t, map) => Id.Map.find_opt(t.id, map.tiles);
let find_shards = (t: Tile.t, map) => Id.Map.find(t.id, map.tiles);

let find_shards' = (id: Id.t, map) =>
  switch (Id.Map.find_opt(id, map.tiles)) {
  | None => []
  | Some(ss) => ss
  };

let find_w = (w: Whitespace.t, map) => Id.Map.find(w.id, map.whitespace);
let find_g = (g: Grout.t, map) => Id.Map.find(g.id, map.grout);
// returns the measurement spanning the whole tile
let find_t = (t: Tile.t, map) => {
  let shards = Id.Map.find(t.id, map.tiles);
  let first = List.assoc(Tile.l_shard(t), shards);
  let last = List.assoc(Tile.r_shard(t), shards);
  {origin: first.origin, last: last.last};
};
// let find_a = ({shards: (l, r), _} as a: Ancestor.t, map) =>
//   List.assoc(l @ r, Id.Map.find(a.id, map.tiles));
let find_p = (p: Piece.t, map) =>
  p
  |> Piece.get(
       w => find_w(w, map),
       g => find_g(g, map),
       t => find_t(t, map),
     );

let union2 = (map: t, map': t) => {
  tiles:
    Id.Map.union((_, ms, ms') => Some(ms @ ms'), map.tiles, map'.tiles),
  grout: Id.Map.union((_, m, _) => Some(m), map.grout, map'.grout),
  whitespace:
    Id.Map.union((_, m, _) => Some(m), map.whitespace, map'.whitespace),
  rows:
    Rows.union(
      (_, s: Rows.shape, s': Rows.shape) =>
        Some({
          indent: min(s.indent, s'.indent),
          max_col: max(s.max_col, s'.max_col),
        }),
      map.rows,
      map'.rows,
    ),
};
let union = List.fold_left(union2, empty);

let post_tile_indent = (t: Tile.t) => {
  // hack for indent following fun/if tiles.
  // proper fix involves updating mold datatype
  // to specify whether a right-facing concave
  // tip imposes indentation on a following newline.
  let complete_fun =
    Tile.is_complete(t)
    && (
      t.label == Form.get("fun_").label || t.label == Form.get("if_").label
    );
  let missing_right_extreme = Tile.r_shard(t) < List.length(t.label) - 1;
  complete_fun || missing_right_extreme;
};

let missing_left_extreme = (t: Tile.t) => Tile.l_shard(t) > 0;

// currently supports indentation imposed by a tile on following
// remainder of segment (eg indentation after fun tile).
// also supports dedenting following encounter with an incomplete tile
// missing its left extreme (eg in when let and = are in backpack),
// but does not current support complete tile dedent, which
// would be desirable for complete tiles in segment remainder that have
// looser precedence than the indent-imposing tile
// TODO: integrate term structure into indentation scheme
let rec of_segment' =
        // start of program is considered a "linebreak"
        // so as to avoid spurious indentation at root level
        (
          ~seen_linebreak=true,
          ~container_indent=0,
          /* indentation imposed by preceding tiles in same segment */
          ~contained_indent=container_indent,
          /* indentation at the start of the row */
          ~row_indent=container_indent,
          ~origin=zero,
          seg: Segment.t,
        )
        : (int, point, t) =>
  switch (seg) {
  | [] => (
      row_indent,
      origin,
      empty |> add_row(origin.row, {indent: row_indent, max_col: origin.col}),
    )
  | [hd, ...tl] =>
    let (seen_linebreak, contained_indent, row_indent, hd_last, hd_map) =
      switch (hd) {
      | Whitespace(w) when w.content == Whitespace.linebreak =>
        let concluding = Segment.sameline_whitespace(tl);
        let indent' =
          if (concluding) {
            container_indent;
          } else if (!seen_linebreak) {
            container_indent + 2;
          } else {
            contained_indent;
          };
        let last = {row: origin.row + 1, col: indent'};
        let map =
          singleton_w(w, {origin, last})
          |> add_row(origin.row, {indent: row_indent, max_col: origin.col});
        (true, indent', indent', last, map);
      | Whitespace(w) =>
        let last = {...origin, col: origin.col + 1};
        (
          seen_linebreak,
          contained_indent,
          row_indent,
          last,
          singleton_w(w, {origin, last}),
        );
      | Grout(g) =>
        let last = {...origin, col: origin.col + 1};
        (
          seen_linebreak,
          contained_indent,
          row_indent,
          last,
          singleton_g(g, {origin, last}),
        );
      | Tile(t) =>
        let token = List.nth(t.label);
        let of_shard = (row_indent, origin, shard) => {
          let last = {
            ...origin,
            col: origin.col + String.length(token(shard)),
          };
          ((row_indent, last), singleton_s(t.id, shard, {origin, last}));
        };
        let ((row_indent, last), map) =
          Aba.mk(t.shards, t.children)
          |> Aba.fold_left_map(
               of_shard(row_indent, origin),
               ((row_indent, origin), child, shard) => {
                 let (row_indent, child_last, child_map) =
                   of_segment'(
                     ~seen_linebreak=false,
                     ~container_indent=contained_indent,
                     ~row_indent,
                     ~origin,
                     child,
                   );
                 let ((row_indent, shard_last), shard_map) =
                   of_shard(row_indent, child_last, shard);
                 ((row_indent, shard_last), child_map, shard_map);
               },
             )
          |> PairUtil.map_snd(Aba.join(Fun.id, Fun.id))
          |> PairUtil.map_snd(union);
        let contained_indent =
          if (post_tile_indent(t)) {
            min(contained_indent + 2, row_indent + 2);
          } else if (missing_left_extreme(t)) {
            container_indent;
          } else {
            contained_indent;
          };
        (seen_linebreak, contained_indent, row_indent, last, map);
      };
    let (row_indent, tl_last, tl_map) =
      of_segment'(
        ~seen_linebreak,
        ~container_indent,
        ~contained_indent,
        ~row_indent,
        ~origin=hd_last,
        tl,
      );
    (row_indent, tl_last, union2(hd_map, tl_map));
  };
let of_segment = seg => {
  let (_, _, map) = of_segment'(seg);
  map;
};

let length = (seg: Segment.t, map: t): int =>
  switch (seg) {
  | [] => 0
  | [p] =>
    let m = find_p(p, map);
    m.last.col - m.origin.col;
  | [hd, ...tl] =>
    let first = find_p(hd, map);
    let last = find_p(ListUtil.last(tl), map);
    last.last.col - first.origin.col;
  };

let segment_origin = (seg: Segment.t): option(point) =>
  Option.map(
    first => find_p(first, of_segment(seg)).origin,
    ListUtil.hd_opt(seg),
  );

let segment_last = (seg: Segment.t): option(point) =>
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
