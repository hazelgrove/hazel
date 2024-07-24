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
  linebreaks: Id.Map.t(rel_indent),
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  secondary: Id.Map.empty,
  projectors: Id.Map.empty,
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
    |> add_row(origin.row + n - 1, {indent: row_indent, max_col: origin.col})
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
let find_shards = (~msg="", t: Tile.t, map) =>
  try(Id.Map.find(t.id, map.tiles)) {
  | _ => failwith("find_shards: " ++ msg)
  };

let find_opt_lb = (id, map) => Id.Map.find_opt(id, map.linebreaks);

let find_shards' = (id: Id.t, map) =>
  switch (Id.Map.find_opt(id, map.tiles)) {
  | None => []
  | Some(ss) => ss
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
  let shards = Id.Map.find(t.id, map.tiles);
  let (first, last) =
    try({
      let first = ListUtil.assoc_err(Tile.l_shard(t), shards, "find_t");
      let last = ListUtil.assoc_err(Tile.r_shard(t), shards, "find_t");
      (first, last);
    }) {
    | _ => failwith("find_t: inconsistent shard infor between tile and map")
    };
  {origin: first.origin, last: last.last};
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
        Some({origin: first.origin, last: last.last});
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

let post_tile_indent = (t: Tile.t) => {
  // hack for indent following fun/if tiles.
  // proper fix involves updating mold datatype
  // to specify whether a right-facing concave
  // tip imposes indentation on a following newline.
  let complete_fun =
    Tile.is_complete(t)
    && (
      t.label == Form.get("fun_").label
      || t.label == Form.get("typfun").label
      || t.label == Form.get("if_").label
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
           | Secondary(w) when Secondary.is_linebreak(w) => (
               false,
               Id.Map.add(w.id, is_indented, map),
             )
           | Secondary(_)
           | Grout(_) => (is_indented, map)
           | Projector(_) => (is_indented, map) // TODO(andrew): ??
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

let last_of_token = (token: string, origin: Point.t): Point.t =>
  // Adjustment for multi-line tokens e.g. projector placeholders
  Point.{
    col: origin.col + StringUtil.max_line_width(token),
    row: origin.row + StringUtil.num_linebreaks(token),
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
        let extra_rows = (token, origin, map) => {
          let row_indent = container_indent + contained_indent;
          let num_extra_rows = StringUtil.num_linebreaks(token);
          add_n_rows(origin, row_indent, num_extra_rows, map);
        };
        let (contained_indent, origin, map) =
          switch (hd) {
          | Secondary(w) when Secondary.is_linebreak(w) =>
            let row_indent = container_indent + contained_indent;
            let indent =
              if (Segment.sameline_secondary(tl)) {
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
          | Secondary(w) =>
            let wspace_length =
              Unicode.length(Secondary.get_string(w.content));
            let last = {...origin, col: origin.col + wspace_length};
            let map = map |> add_w(w, {origin, last});
            (contained_indent, last, map);
          | Grout(g) =>
            let last = {...origin, col: origin.col + 1};
            let map = map |> add_g(g, {origin, last});
            (contained_indent, last, map);
          | Projector(p) =>
            let token = Projector.placeholder_str(p);
            let last = last_of_token(token, origin);
            let map = extra_rows(token, origin, map);
            let map = add_pr(p, {origin, last}, map);
            (contained_indent, last, map);
          | Tile(t) =>
            let add_shard = (origin, shard, map) => {
              let token = List.nth(t.label, shard);
              let map = extra_rows(token, origin, map);
              let last = last_of_token(token, origin);
              let map = add_s(t.id, shard, {origin, last}, map);
              (last, map);
            };
            //TODO(andrew)
            // let add_shard = (origin, shard, map) => {
            //   let token = List.nth(t.label, shard);
            //   // Adjustment for multi-line tokens e.g. projector placeholders
            //   let last =
            //     Point.{
            //       col: origin.col + StringUtil.max_line_width(token),
            //       row: origin.row + StringUtil.num_linebreaks(token),
            //     };
            //   let map = map |> add_s(t.id, shard, {origin, last});
            //   let row_indent = container_indent + contained_indent;
            //   let rec add_n_rows = (n, map) =>
            //     switch (n) {
            //     | 0 => map
            //     | _ =>
            //       map
            //       |> add_n_rows(n - 1)
            //       |> add_row(
            //            origin.row + n - 1,
            //            {indent: row_indent, max_col: origin.col},
            //          )
            //     };
            //   let map = map |> add_n_rows(StringUtil.num_linebreaks(token));
            //   (last, map);
            // };
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
