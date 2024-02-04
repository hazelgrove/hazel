open Sexplib.Std;
open Util;

module Int_ppx = {
  include Int;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  module Map = IntMap;
};
module Row = Int_ppx;
module Col = Int_ppx;

module Point = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    row: Row.t,
    col: Col.t,
  };
  let zero = {row: 0, col: 0};

  let compare = (l, r) => {
    let c = Row.compare(l.row, r.row);
    c == 0 ? Col.compare(l.col, r.col) : c;
  };

  type comparison =
    | Exact
    | Under
    | Over;
  // let comp = (current, target): comparison =>
  //   switch () {
  //   | _ when current == target => Exact
  //   | _ when current < target => Under
  //   | _ => Over
  //   };
  // let compare = (p1, p2) =>
  //   switch (comp(p1, p2)) {
  //   | Exact => 0
  //   | Under => (-1)
  //   | Over => 1
  //   };
  // let dcomp = (d: Dir.t, x, y) =>
  //   switch (d) {
  //   | R => comp(x, y)
  //   | L => comp(y, x)
  //   };
};

module Range = {
  type t = (Point.t, Point.t);
};

module Rows = {
  include IntMap;
  type shape = {
    indent: Col.t,
    max_col: Col.t,
  };
  type t = Row.Map.t(shape);

  let max_col = (rs: list(Row.t), map: t) =>
    rs |> List.map(r => find(r, map).max_col) |> List.fold_left(max, 0);

  let min_col = (rs: list(Row.t), map: t) =>
    rs
    |> List.map(r => find(r, map).indent)
    |> List.fold_left(min, Int.max_int);
};

type t = {
  toks: Id.Map.t(Range.t),
  rows: Rows.t,
};

let empty = {toks: Id.Map.empty, rows: Rows.empty};

let union2 = (l: t, r: t) => {
  toks: Id.Map.union((_, m, _) => Some(m), l.toks, r.toks),
  rows:
    Rows.union(
      (_, s: Rows.shape, s': Rows.shape) =>
        Some({
          indent: min(s.indent, s'.indent),
          max_col: max(s.max_col, s'.max_col),
        }),
      l.rows,
      r.rows,
    ),
};
let union = List.fold_left(union2, empty);

// module Traversal = {
//   type map = t;
//   type t = {
//     map,
//     indent: int,
//     origin: Point.t,
//   };
//   let init = {map: empty, indent: 0, origin: Point.zero};

//   // todo
//   let is_indenting = _ => false;

//   let add_space = (~to_be_indented=false, s: Space.t, state: t): t =>
//     s.chars
//     |> List.fold_left(
//          (({map, indent, origin}, indented), s: Space.Char.t) =>
//            switch (s.shape) {
//            | Space =>
//              let last = {...origin, col: origin.col + 1};
//              let map = add_space(s, {origin, last}, map);
//              ({map, indent, origin: last}, indented);
//            | Newline =>
//              let (indent, indented) =
//                to_be_indented && !indented
//                  ? (indent + 2, true) : (indent, indented);
//              let last = Point.{row: origin.row + 1, col: indent};
//              let map = add_space(s, {origin, last}, map);
//              ({map, indent, origin: last}, indented);
//            },
//          (state, false),
//        )
//     |> fst;

//   let rec add_meld = (~to_be_indented=false, mel: Meld.t, state: t): t =>
//     state
//     |> add_space(~to_be_indented, fst(mel.space))
//     |> (
//       switch (mel.chain) {
//       | None => Fun.id
//       | Some(c) => add_chain(c)
//       }
//     )
//     |> add_space(snd(mel.space))
//   and add_chain = (c, state) =>
//     c
//     |> Chain.fold_left(
//          kid => add_meld(kid, state),
//          (state, p, kid) =>
//            state
//            |> add_piece(p)
//            |> add_meld(~to_be_indented=is_indenting(p), kid),
//        )
//   and add_piece = (p: Piece.t, state: t) => {
//     let origin = state.origin;
//     let last = {...origin, col: origin.col + Piece.length(p)};
//     let map = state.map |> add_p(p, {origin, last});
//     {...state, map, origin: last};
//   };
// };
