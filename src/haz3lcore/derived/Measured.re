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

let rec add_n_rows = (origin: Point.t, row_indent, n, map: t): t =>
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

type acc = (int, Point.t, t);

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
};

let of_segment =
    (
      ~indent_level=Id.Map.empty,
      seg: Segment.t,
      shape_map: Id.Map.t(ProjectorCore.Shape.t),
    )
    : t => {
  let indent_level =
    Id.Map.is_empty(indent_level)
      ? Indentation.level_map(seg) : indent_level;

  let indent_of_linebreak = (w: Secondary.t): option(int) =>
    Secondary.is_linebreak(w) ? Id.Map.find_opt(w.id, indent_level) : None;

  module DeferredLinebreaks = MkDeferredLinebreaks();

  let projector_size =
      (p: Base.projector, shape_map: Id.Map.t(ProjectorCore.Shape.t))
      : Point.t => {
    let shape = ProjectorCore.Shape.Map.lookup(p.id, shape_map);
    let row =
      switch (shape.vertical) {
      | Inline
      | Block(0) => 0
      | Tab(num_lb) =>
        DeferredLinebreaks.update(num_lb);
        0;
      | Block(num_lb) => max(num_lb, DeferredLinebreaks.consume())
      };
    {
      col: shape.horizontal,
      row,
    };
  };

  let calc = (indent: int, origin: Point.t, map: t, size: Point.t) => {
    let last = Point.add(origin, size);
    let map = add_n_rows(origin, indent, size.row, map);
    (mk_measurement(origin, last), map);
  };

  let add_shard = ((indent, origin, map): acc, t: Tile.t, idx: int) => {
    let size = Token.bounding_box(List.nth(t.label, idx));
    let (measure, map) = calc(indent, origin, map, size);
    (indent, measure.last, add_s(t.id, idx, measure, map));
  };

  let add_grout = ((indent, origin, map): acc, g: Grout.t) => {
    let size = Point.mk(~row=0, ~col=1);
    let (measure, map) = calc(indent, origin, map, size);
    (indent, measure.last, add_g(g, measure, map));
  };

  let add_projector = ((indent, origin, map): acc, pr: Base.projector) => {
    let size = projector_size(pr, shape_map);
    let (measure, map) = calc(indent, origin, map, size);
    (indent, measure.last, add_pr(pr, measure, map));
  };

  let add_secondary = ((prev_indent, origin, map): acc, w: Secondary.t) => {
    let (new_indent, size) =
      switch (indent_of_linebreak(w)) {
      | Some(new_indent) =>
        let size =
          Point.mk(
            ~row=1 + DeferredLinebreaks.consume(),
            ~col=new_indent - origin.col,
          );
        (new_indent, size);
      | None =>
        let size = Point.mk(~row=0, ~col=Secondary.length(w));
        (prev_indent, size);
      };
    let (measure, map) = calc(prev_indent, origin, map, size);
    (new_indent, measure.last, add_w(w, measure, map));
  };

  let add_top_level = ((indent, origin, map): acc, ~top_level: bool) => {
    let map =
      top_level
        ? add_n_rows(origin, indent, 1 + DeferredLinebreaks.consume(), map)
        : map;
    (indent, origin, map);
  };

  let rec go = (acc: acc, ~top_level: bool, seg: Segment.t): acc =>
    switch (seg) {
    | [] => add_top_level(acc, ~top_level)
    | [hd, ...tl] =>
      let acc =
        switch (hd) {
        | Secondary(w) => add_secondary(acc, w)
        | Grout(g) => add_grout(acc, g)
        | Projector(p) => add_projector(acc, p)
        | Tile(t) =>
          Aba.mk(t.shards, t.children)
          |> Aba.fold_left(add_shard(acc, t), (acc, child, idx: int) =>
               add_shard(go(acc, ~top_level=false, child), t, idx)
             )
        };
      go(acc, ~top_level, tl);
    };
  let (_, _, map) = go((0, Point.zero, empty), ~top_level=true, seg);
  map;
};

/* Memoized for perf */
let of_segment = Core.Memo.general(of_segment);

/* Width in characters of row at measurement.origin */
let start_row_width = (measurement: measurement, measured: t): int =>
  switch (IntMap.find_opt(measurement.origin.row, measured.rows)) {
  | None => 0
  | Some(row) => row.max_col
  };
