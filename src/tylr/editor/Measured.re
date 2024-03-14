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

  let add = (~return: Col.t, p: t, dims: Dims.t) => {
    row: p.row + dims.height,
    col: (dims.height > 0 ? return : p.col) + dims.width,
  };

  let compare = (l, r) => {
    let c = Row.compare(l.row, r.row);
    c == 0 ? Col.compare(l.col, r.col) : c;
  };

  let eq = (==);
  let lt = (l, r) => compare(l, r) < 0;
};

module Traversed = {
  type t = {
    // indent level of current row
    indent: Col.t,
    // current row-col position
    point: Point.t,
  };

  let mk = (~indent=0, ~return=indent, ~point=Point.zero, ()) => {
    indent,
    return,
    point,
  };
  let init = mk();
};
module Indented = {
  // indentation of...
  type t = {
    // ...current row
    curr: Col.t,
    // ...next row
    next: Col.t,
    // ...next row whose first token delimits end of current cell
    end_: Col.t,
  };
  let mk = (~curr=0, ~next=curr, ~end_=curr, ()) => {curr, next, end_};
  let init = mk();

  let isize = 2;

  let push =
      (~null as (l, r)=(false, false), s: Bound.t(Mtrl.PSort.t), i: t) =>
    switch (s) {
    | Root
    | Node(Space | Tile(({indent: false, _}, _))) => i
    | Node(Grout | Tile(({indent: true, _}, _))) => {
        ...i,
        next: (l ? i.next : i.curr) + isize,
        end_: r ? i.end_ : i.curr,
      }
    };
};

let path_of_point = (target: Pos.t, c: Cell.t): Path.Point.t => {
  let rec go =
          (
            ~null=(false, false),
            ~pos=Pos.zero,
            ~ind=Indented.init,
            cell: Cell.t,
          )
          : Path.Point.t => {
    let indent =
      cell.sort |> Bound.map(Molded.Sort.indent) |> Bound.get(~root=false);
    switch (Cell.get(cell)) {
    | None => Path.Point.here
    | Some(M(l, (ts, cs), r)) =>
      open Result.Syntax;
      let ind =
        Indented.push(~null, Bound.map(Molded.mtrl_, cell.sort), ind);
      let add = Pos.add(~indent=ind.curr);
      let l_end = Pos.add(pos, Dims.of_cell(l));
      let/ l_end =
        if (Pos.lt(l_end, target)) {
          Error(l_end);
        } else if (Pos.eq(l_end, target)) {
          Ok(Path.Point.mk(Tok(0, 0)));
        } else {
          go(~null=(true, false), ~pos, ~ind, target, l)
          |> Path.Point.cons(0);
        };
      let its = List.mapi((i, t) => (i, t), ts);
      let/ w_end =
        (its, cs)
        |> Chain.fold(
             ((i, tok)) => {
               let t_end = add(l_end, Dims.of_tok(tok));
               Pos.lt(t_end, target)
                 ? Error(t_end)
                 : Ok(Path.Point.mk(Tok(i, target.col - l_end.col)));
             },
             (found, c, (i, t)) => {
               let/ pos = found;
               let c_end = Point.add(pos, Dims.of_cell(c));
               let/ c_end =
                 if (Pos.lt(c_end, target)) {
                   Error(c_end);
                 } else if (Pos.eq(c_end, target)) {
                   Ok(Path.Point.mk(Tok(i, 0)));
                 } else {
                   go(~pos, ~ind, c);
                 };
               let t_end = add(c_end, Dims.of_tok(tok));
               Pos.lt(t_end, target)
                 ? Error(t_end)
                 : Ok(Path.Point.mk(Tok(i, target.col - c_end.col)));
             },
           );
      let r_end = Pos.add(pos, Dims.of_cell(r));
      if (Pos.lt(r_end, target)) {
        Error(r_end);
      } else if (Pos.eq(r_end, target)) {
        Ok(Path.Point.mk(End(R)));
      } else {
        go(~null=(false, true), ~pos, ~ind, target, r)
        |> Path.Point.cons(List.length(ts));
      };
    };
  };
  switch (go(c)) {
  | Ok(path) => path
  | Error(_) => Path.Point.mk(End(R))
  };
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
