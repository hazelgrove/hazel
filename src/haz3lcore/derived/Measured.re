open Util;
open Point;

module Point = Point;

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

type t = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  secondary: Id.Map.t(measurement),
  projectors: Id.Map.t(measurement),
  rows: Rows.t,
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  secondary: Id.Map.empty,
  projectors: Id.Map.empty,
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
let add_pr = (p: Base.projector, m, map) => {
  ...map,
  projectors: map.projectors |> Id.Map.add(p.id, m),
};
let add_p = (p: Piece.t, m, map) =>
  p
  |> Piece.get(
       w => add_w(w, m, map),
       g => add_g(g, m, map),
       t => add_t(t, m, map),
       pr => add_pr(pr, m, map),
     );

let add_row = (row: int, shape: Rows.shape, map) => {
  ...map,
  rows: Rows.add(row, shape, map.rows),
};

let rec add_n_rows = (origin: Point.t, row_indent, n: abs_indent, map: t): t =>
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

let find_shards = (~msg="", t: Tile.t, map) =>
  try(Id.Map.find(t.id, map.tiles)) {
  | _ => failwith("find_shards: " ++ msg)
  };
let find_w = (~msg="", w: Secondary.t, map): measurement =>
  try(Id.Map.find(w.id, map.secondary)) {
  | _ => failwith("find_w: " ++ msg)
  };
let find_g = (~msg="", g: Grout.t, map): measurement =>
  try(Id.Map.find(g.id, map.grout)) {
  | _ => failwith("find_g: " ++ msg)
  };
let find_pr = (~msg="", p: Base.projector, map): measurement =>
  try(Id.Map.find(p.id, map.projectors)) {
  | _ => failwith("find_g: " ++ msg)
  };
let find_pr_opt = (p: Base.projector, map): option(measurement) =>
  Id.Map.find_opt(p.id, map.projectors);
// returns the measurement spanning the whole tile
let find_t = (t: Tile.t, map): measurement => {
  let shards = find_shards(t, map);
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
let find_p = (~msg="", p: Piece.t, map): measurement =>
  try(
    p
    |> Piece.get(
         w => find_w(w, map),
         g => find_g(g, map),
         t => find_t(t, map),
         p => find_pr(p, map),
       )
  ) {
  | _ => failwith("find_p: " ++ msg ++ "id: " ++ Id.to_string(p |> Piece.id))
  };

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

/* Tab projectors add linebreaks after the end of their line */
let deferred_linebreaks: ref(int) = ref(0);

let consume_deferred_linebreaks = (): int => {
  let ret = deferred_linebreaks^;
  deferred_linebreaks := 0;
  ret;
};

let of_segment =
    (
      ~indent_level=Id.Map.empty,
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorShape.t),
    )
    : t => {
  let indent_level =
    Id.Map.is_empty(indent_level)
      ? Indentation.level_map(seg) : indent_level;

  deferred_linebreaks := 0;

  let rec go =
          (
            ~map,
            ~prev_indent: int,
            ~top_level,
            ~origin: Point.t,
            seg: Segment.t,
          )
          : (int, Point.t, t) =>
    switch (seg) {
    | [] =>
      let map =
        if (top_level) {
          add_n_rows(
            origin,
            prev_indent,
            1 + consume_deferred_linebreaks(),
            map,
          );
        } else {
          map;
        };
      (prev_indent, origin, map);
    | [hd, ...tl] =>
      let (prev_indent, origin, map) =
        switch (hd) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          let indent =
            switch (Id.Map.find_opt(w.id, indent_level)) {
            | Some(indent) => indent
            | None => 0
            };
          let num_extra_rows = 1 + consume_deferred_linebreaks();
          let last =
            Point.{
              row: origin.row + num_extra_rows,
              col: indent,
            };
          let map =
            map
            |> add_w(
                 w,
                 {
                   origin,
                   last,
                 },
               )
            |> add_n_rows(origin, prev_indent, num_extra_rows);
          (indent, last, map);
        | Secondary(w) =>
          let last = {
            ...origin,
            col:
              origin.col + Unicode.length(Secondary.get_string(w.content)),
          };
          let map =
            add_w(
              w,
              {
                origin,
                last,
              },
              map,
            );
          (prev_indent, last, map);
        | Grout(g) =>
          let last = {
            ...origin,
            col: origin.col + 1,
          };
          let map =
            add_g(
              g,
              {
                origin,
                last,
              },
              map,
            );
          (prev_indent, last, map);
        | Projector(p) =>
          let indent =
            switch (Id.Map.find_opt(p.id, indent_level)) {
            | Some(indent) => indent
            | None => 0
            };
          let shape = ProjectorShape.Map.lookup(p.id, shape_map);
          let num_extra_rows =
            switch (shape.vertical) {
            | Inline
            | Tab(0)
            | Block(0) => 0
            | Tab(num_lb) =>
              deferred_linebreaks := max(num_lb, deferred_linebreaks^);
              num_lb;
            | Block(num_lb) => num_lb + consume_deferred_linebreaks()
            };
          let last = {
            col: origin.col + shape.horizontal,
            row:
              switch (shape.vertical) {
              | Inline => origin.row
              | Tab(_) => origin.row
              | Block(num_lb) => origin.row + num_lb
              },
          };
          let map =
            map
            |> add_n_rows(origin, indent, num_extra_rows)
            |> add_pr(
                 p,
                 {
                   origin,
                   last,
                 },
               );
          (indent, last, map);
        | Tile(t) =>
          let last_of_token = (token: string, origin: Point.t): Point.t => {
            col: origin.col + StringUtil.max_line_width(token),
            row: origin.row + StringUtil.num_linebreaks(token),
          };
          let add_shard = (prev_indent, origin, map, shard) => {
            let token = List.nth(t.label, shard);
            let last = last_of_token(token, origin);
            let map =
              add_s(
                t.id,
                shard,
                {
                  origin,
                  last,
                },
                map,
              );
            (prev_indent, last, map);
          };
          Aba.mk(t.shards, t.children)
          |> Aba.fold_left(
               add_shard(prev_indent, origin, map),
               ((_prev_indent, origin, map), child, shard) => {
                 let (prev_indent, child_last, child_map) =
                   go(~map, ~prev_indent, ~top_level=false, ~origin, child);
                 add_shard(prev_indent, child_last, child_map, shard);
               },
             );
        };
      go(~map, ~prev_indent, ~top_level, ~origin, tl);
    };
  let (_, _, map) =
    go(~map=empty, ~prev_indent=0, ~top_level=true, ~origin=Point.zero, seg);
  map;
};

/* Memoized for perf */
let of_segment = Core.Memo.general(of_segment, _);

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

let width = (seg: Segment.t, map: t): int => {
  let first = find_p(List.hd(seg), map);
  let last = find_p(ListUtil.last(seg), map);
  let rows =
    List.init(last.last.row - first.origin.row + 1, i => first.origin.row + i);
  Rows.max_col(rows, map.rows);
};

let height = (seg: Segment.t, map: t): int =>
  switch (seg) {
  | [] => 0
  | [p] =>
    let m = find_p(p, map);
    m.last.row - m.origin.row;
  | [hd, ...tl] =>
    let first = find_p(hd, map);
    let last = find_p(ListUtil.last(tl), map);
    last.last.row - first.origin.row;
  };

/* Width in characters of row at measurement.origin */
let start_row_width = (measurement: measurement, measured: t): int =>
  switch (IntMap.find_opt(measurement.origin.row, measured.rows)) {
  | None => 0
  | Some(row) => row.max_col
  };
