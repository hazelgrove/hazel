open Util;

// [@deriving (show({with_path: false}), sexp, yojson)]
type row = int;
// [@deriving (show({with_path: false}), sexp, yojson)]
type col = int;

module Point = {
  // [@deriving (show({with_path: false}), sexp, yojson)]
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

  let dcomp = (d: Dir.t, x, y) =>
    switch (d) {
    | R => comp(x, y)
    | L => comp(y, x)
    };
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

// [@deriving (show({with_path: false}), sexp, yojson)]
type measurement = {
  origin: Point.t,
  last: Point.t,
};

type t = {
  tiles: Id.Map.t(measurement),
  grout: Id.Map.t(measurement),
  space: Id.Map.t(measurement),
  rows: Rows.t,
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  space: Id.Map.empty,
  rows: Rows.empty,
};

let union2 = (map: t, map': t) => {
  tiles: Id.Map.union((_, m, _) => Some(m), map.tiles, map'.tiles),
  grout: Id.Map.union((_, m, _) => Some(m), map.grout, map'.grout),
  space: Id.Map.union((_, m, _) => Some(m), map.space, map'.space),
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

let add_p = (_, _, _) => failwith("todo");
let add_space = (_, _, _) => failwith("todo");

type state = {
  map: t,
  indent: int,
  origin: Point.t,
};
let init = {map: empty, indent: 0, origin: Point.zero};

// todo
let tile_indent = _ => false;

let of_space = (~tile_indent=false, ~state, s: Space.t): state =>
  s
  |> List.fold_left(
       (({map, indent, origin}, indented), elem: Space.elem) =>
         switch (elem.shape) {
         | Space =>
           let last = {...origin, col: origin.col + 1};
           let map = add_space(elem, {origin, last}, map);
           ({map, indent, origin: last}, indented);
         | Newline =>
           let (indent, indented) =
             tile_indent && !indented
               ? (indent + 2, true) : (indent, indented);
           let last = Point.{row: origin.row + 1, col: indent};
           let map = add_space(elem, {origin, last}, map);
           ({map, indent, origin: last}, indented);
         },
       (state, false),
     )
  |> fst;

let rec of_chain = (~state=init, c: Chain.t) =>
  c
  |> Aba.fold_left(
       of_kid(~state),
       (state, p, k) => {
         let state = of_piece(~state, p);
         of_kid(~state, k);
       },
     )
and of_kid = (~state, k: option(Chain.kid)) =>
  switch (k) {
  | None => state
  | Some(K(kid)) => of_chain(~state, kid)
  }
and of_piece = (~state: state, p: Piece.t) => {
  let (l, r) = p.space;
  let state = of_space(~state, l);
  let state = {
    let origin = state.origin;
    let last = {...origin, col: origin.col + Piece.length(p)};
    let map = state.map |> add_p(p, {origin, last});
    {...state, map, origin: last};
  };
  of_space(~state, ~tile_indent=tile_indent(p), r);
};

// let rec of_segment =
//         (~indent=0, ~origin=Point.zero, seg: Segment.t): (Point.t, t) =>
//   failwith("todo");
