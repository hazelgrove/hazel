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
  linebreaks: Id.Map.t(rel_indent),
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  whitespace: Id.Map.empty,
  rows: Rows.empty,
  linebreaks: Id.Map.empty,
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

let add_lb = (id, indent, map) => {
  ...map,
  linebreaks: Id.Map.add(id, indent, map.linebreaks),
};

let singleton_w = (w, m) => empty |> add_w(w, m);
let singleton_g = (g, m) => empty |> add_g(g, m);
let singleton_s = (id, shard, m) => empty |> add_s(id, shard, m);

// TODO(d) rename
let find_opt_shards = (t: Tile.t, map) => Id.Map.find_opt(t.id, map.tiles);
let find_shards = (t: Tile.t, map) => Id.Map.find(t.id, map.tiles);

let find_opt_lb = (id, map) => Id.Map.find_opt(id, map.linebreaks);

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
  linebreaks:
    Id.Map.union((_, i, _) => Some(i), map.linebreaks, map'.linebreaks),
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

let is_indented_map = (seg: Segment.t) => {
  let rec go = (~is_indented=false, ~map=Id.Map.empty, seg: Segment.t) =>
    seg
    |> List.fold_left(
         ((is_indented, map), p: Piece.t) =>
           switch (p) {
           | Whitespace(w) when Whitespace.is_linebreak(w) => (
               false,
               Id.Map.add(w.id, is_indented, map),
             )
           | Whitespace(_)
           | Grout(_) => (is_indented, map)
           | Tile(t) =>
             let is_indented = is_indented || post_tile_indent(t);
             let map =
               t.children
               |> List.fold_left(
                    (map, child) => go(~is_indented=true, ~map, child),
                    map,
                  );
             (is_indented, map);
           },
         (is_indented, map),
       )
    |> snd;
  go(seg);
};

let of_segment = (~old: t=empty, ~touched=Touched.empty, seg: Segment.t): t => {
  let is_indented = is_indented_map(seg);

  // recursive across seg's bidelimited containers
  let rec go_nested =
          (
            ~map,
            ~container_indent: abs_indent=0,
            ~origin=Point.zero,
            seg: Segment.t,
          )
          : (Point.t, t) => {
    let first_touched_incomplete =
      switch (Segment.incomplete_tiles(seg)) {
      | [] => None
      | ts =>
        ts
        |> List.map((t: Tile.t) => Touched.find_opt(t.id, touched))
        |> List.fold_left(
             (acc, touched) =>
               switch (acc, touched) {
               | (Some(time), Some(time')) => Some(Time.min(time, time'))
               | (Some(time), _)
               | (_, Some(time)) => Some(time)
               | _ => None
               },
             None,
           )
      };

    // recursive across seg's list structure
    let rec go_seq =
            (
              ~map,
              ~contained_indent: rel_indent=0,
              ~origin: Point.t,
              seg: Segment.t,
            )
            : (Point.t, t) =>
      switch (seg) {
      | [] =>
        let map =
          map
          |> add_row(
               origin.row,
               {
                 indent: container_indent + contained_indent,
                 max_col: origin.col,
               },
             );
        (origin, map);
      | [hd, ...tl] =>
        let (contained_indent, origin, map) =
          switch (hd) {
          | Whitespace(w) when Whitespace.is_linebreak(w) =>
            let row_indent = container_indent + contained_indent;
            let indent =
              if (Segment.sameline_whitespace(tl)) {
                0;
              } else {
                switch (
                  Touched.find_opt(w.id, touched),
                  first_touched_incomplete,
                  find_opt_lb(w.id, old),
                ) {
                | (Some(touched), Some(touched'), Some(indent))
                    when Time.lt(touched, touched') => indent
                | _ =>
                  contained_indent + (Id.Map.find(w.id, is_indented) ? 2 : 0)
                };
              };
            let last =
              Point.{row: origin.row + 1, col: container_indent + indent};
            let map =
              map
              |> add_w(w, {origin, last})
              |> add_row(
                   origin.row,
                   {indent: row_indent, max_col: origin.col},
                 )
              |> add_lb(w.id, indent);
            (indent, last, map);
          | Whitespace(w) =>
            let wspace_length =
              Unicode.length(Whitespace.get_string(w.content));
            let last = {...origin, col: origin.col + wspace_length};
            let map = map |> add_w(w, {origin, last});
            (contained_indent, last, map);
          | Grout(g) =>
            let last = {...origin, col: origin.col + 1};
            let map = map |> add_g(g, {origin, last});
            (contained_indent, last, map);
          | Tile(t) =>
            let token = List.nth(t.label);
            let add_shard = (origin, shard, map) => {
              let last =
                Point.{
                  ...origin,
                  col: origin.col + String.length(token(shard)),
                };
              let map = map |> add_s(t.id, shard, {origin, last});
              (last, map);
            };
            let (last, map) =
              Aba.mk(t.shards, t.children)
              |> Aba.fold_left(
                   shard => add_shard(origin, shard, map),
                   ((origin, map), child, shard) => {
                     let (child_last, child_map) =
                       go_nested(
                         ~map,
                         ~container_indent=container_indent + contained_indent,
                         ~origin,
                         child,
                       );
                     add_shard(child_last, shard, child_map);
                   },
                 );
            (contained_indent, last, map);
          };
        let (tl_last, map) = go_seq(~map, ~contained_indent, ~origin, tl);
        (tl_last, map);
      };
    go_seq(~map, ~origin, seg);
  };
  snd(go_nested(~map=empty, seg));
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
