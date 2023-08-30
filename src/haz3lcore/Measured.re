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
  secondary: Id.Map.t(measurement),
  rows: Rows.t,
  linebreaks: Id.Map.t(rel_indent),
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  secondary: Id.Map.empty,
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

let find_w = (w: Secondary.t, map): measurement =>
  Id.Map.find(w.id, map.secondary);
let find_g = (g: Grout.t, map): measurement => Id.Map.find(g.id, map.grout);
// returns the measurement spanning the whole tile
let find_t = (t: Tile.t, map): measurement => {
  let shards = Id.Map.find(t.id, map.tiles);
  let first = ListUtil.assoc_err(Tile.l_shard(t), shards, "find_t");
  let last = ListUtil.assoc_err(Tile.r_shard(t), shards, "find_t");
  {origin: first.origin, last: last.last};
};
// let find_a = ({shards: (l, r), _} as a: Ancestor.t, map) =>
//   List.assoc(l @ r, Id.Map.find(a.id, map.tiles));
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

let is_incrementor = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label, _} as t) when Tile.is_complete(t) =>
    switch (label) {
    | ["fun", "->"] => true
    | ["if", "then", "else"] => true
    | ["|", "=>"] => true
    | _ => false
    }
  /*| Tile({label, shards, _}) =>
    /* Incomplete case: does the incomplete tile end
     * in what will become a bidelimited context? */
    List.length(label) == 2
    && shards == [0]
    || List.length(label) == 3
    && shards == [0]
    || List.length(label) == 3
    && shards == [0, 1]*/
  | _ => false
  };

let is_resetter = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label, _} as t) when Tile.is_complete(t) =>
    switch (label) {
    | ["|", "=>"] => true
    | _ => false
    }
  | _ => false
  };

let trim_non_things = (seg: Segment.t) => {
  /* For our purposes here, non-things are spaces and concave grout */
  let rec trim_l: list(Base.piece) => list(Base.piece) =
    xs =>
      switch (xs) {
      | [] => []
      | [Grout({shape: Concave, _}), ...xs] => trim_l(xs)
      | [Secondary(s), ...xs] when Secondary.is_space(s) => trim_l(xs)
      | [_, ..._] => xs
      };
  Segment.trim_f(trim_l, Left, seg);
};

let last_thing_before = (idx: int, seg: Segment.t): option(Piece.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((pre, _, _)) =>
    switch (pre |> List.rev |> trim_non_things) {
    | [] => None
    | [p, ..._] => Some(p)
    }
  | _ => None
  };

let next_thing_after = (idx: int, seg: Segment.t): option(Piece.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((_, _, suf)) =>
    switch (suf |> trim_non_things) {
    | [] => None
    | [p, ..._] => Some(p)
    }
  | _ => None
  };

let prev_thing_is_incrementor = (idx: int, seg: Segment.t): bool => {
  switch (last_thing_before(idx, seg)) {
  | Some(p) => is_incrementor(p)
  | None => false
  };
};

let next_thing_is_resetter = (idx: int, seg: Segment.t) =>
  switch (next_thing_after(idx, seg)) {
  | Some(p) => is_resetter(p)
  | None => false
  };

let is_first_thing = (idx: int, seg: Segment.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((pre, _, _)) =>
    switch (pre |> trim_non_things) {
    | [] => true
    | _ => false
    }
  | None => false
  };

let is_last_thing = (idx: int, seg: Segment.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((_, _, suf)) =>
    switch (suf |> trim_non_things) {
    | [] => true
    | _ => false
    }
  | None => false
  };

let indent_level_map = (seg: Segment.t): Id.Map.t(int) => {
  let rec go =
          (base_level: int, map: Id.Map.t(int), seg: Segment.t)
          : (int, Id.Map.t(int)) => {
    List.fold_left2(
      ((level: int, map: Id.Map.t(int)), p: Piece.t, idx: int) => {
        switch (p) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          let level =
            is_first_thing(idx, seg) || prev_thing_is_incrementor(idx, seg)
              ? level + 2
              : is_last_thing(idx, seg) || next_thing_is_resetter(idx, seg)
                  ? base_level : level;
          let map = Id.Map.add(w.id, level, map);
          (level, map);
        | Secondary(_)
        | Grout(_) => (level, map)
        | Tile(t) =>
          let map =
            List.fold_left(
              (map, seg) => go(level, map, seg) |> snd,
              map,
              t.children,
            );
          (level, map);
        }
      },
      (base_level, map),
      seg,
      List.init(List.length(seg), Fun.id),
    );
  };
  go(0, Id.Map.empty, seg) |> snd;
};

let of_segment =
    (
      ~indent_level=Id.Map.empty,
      ~old as _: t=empty,
      ~touched as _=Touched.empty,
      seg: Segment.t,
    )
    : t => {
  let indent_level =
    Id.Map.is_empty(indent_level) ? indent_level_map(seg) : indent_level;
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
            let indent =
              try(Id.Map.find(w.id, indent_level)) {
              | _ =>
                Printf.printf(
                  "ERROR: Measured ID not found: %s\n",
                  Id.to_string(w.id),
                );
                0;
              };
            let last = Point.{row: origin.row + 1, col: indent};
            let map =
              map
              |> add_w(w, {origin, last})
              |> add_row(
                   origin.row,
                   {indent: prev_indent, max_col: origin.col},
                 )
              |> add_lb(w.id, 666);
            (indent, last, map);
          | Secondary(w) =>
            let wspace_length =
              Unicode.length(Secondary.get_string(w.content));
            let last = {...origin, col: origin.col + wspace_length};
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
