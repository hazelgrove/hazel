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

let add_p = (_, _, _) => failwith("todo add_p");
let add_space = (_, _, _) => failwith("todo add_space");

module Traversal = {
  type map = t;
  type t = {
    map,
    indent: int,
    origin: Point.t,
  };
  let init = {map: empty, indent: 0, origin: Point.zero};

  // todo
  let is_indenting = _ => false;

  let add_space = (~to_be_indented=false, s: Space.t, state: t): t =>
    s.chars
    |> List.fold_left(
         (({map, indent, origin}, indented), s: Space.Char.t) =>
           switch (s.shape) {
           | Space =>
             let last = {...origin, col: origin.col + 1};
             let map = add_space(s, {origin, last}, map);
             ({map, indent, origin: last}, indented);
           | Newline =>
             let (indent, indented) =
               to_be_indented && !indented
                 ? (indent + 2, true) : (indent, indented);
             let last = Point.{row: origin.row + 1, col: indent};
             let map = add_space(s, {origin, last}, map);
             ({map, indent, origin: last}, indented);
           },
         (state, false),
       )
    |> fst;

  let rec add_meld = (~to_be_indented=false, mel: Meld.t, state: t): t =>
    state
    |> add_space(~to_be_indented, fst(mel.space))
    |> (
      switch (mel.chain) {
      | None => Fun.id
      | Some(c) => add_chain(c)
      }
    )
    |> add_space(snd(mel.space))
  and add_chain = (c, state) =>
    c
    |> Chain.fold_left(
         kid => add_meld(kid, state),
         (state, p, kid) =>
           state
           |> add_piece(p)
           |> add_meld(~to_be_indented=is_indenting(p), kid),
       )
  and add_piece = (p: Piece.t, state: t) => {
    let origin = state.origin;
    let last = {...origin, col: origin.col + Piece.length(p)};
    let map = state.map |> add_p(p, {origin, last});
    {...state, map, origin: last};
  };
};
