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

module Pos = {
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

  let eq = (l, r) => compare(l, r) == 0;
  let lt = (l, r) => compare(l, r) < 0;
  let leq = (l, r) => compare(l, r) <= 0;
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

  let indent_next = ind => {...ind, next: ind.curr + isize};
};

// returns a valid path into c whose pos is nearest the given target,
// where nearest is defined by the ordering relation Pos.lt
let path_of_pos = (target: Pos.t, c: Cell.t): Path.Point.t => {
  let rec go =
          (
            ~null=(true, true),
            ~ind=Indented.init,
            ~pos=Pos.zero,
            cell: Cell.t,
          )
          : Result.t(Path.Point.t, Pos.t) =>
    switch (Cell.get(cell)) {
    | None => Ok(Path.Point.here)
    | Some(M(l, W((ts, cs)), r)) =>
      open Result.Syntax;
      // let skip_or_go = skip_or_go(~ind, ~pos);
      // let skip_or_arrive
      let/ pos = go_cell(~null=(true, false), ~ind, ~pos, 0, l);
      let/ (ind, pos) =
        (List.mapi((i, t) => (i, t), ts), cs)
        |> Chain.fold_left(
             ((i, t)) => go_tok(~ind, ~pos, i, t),
             (went, c, (i, t)) => {
               let/ (ind, pos) = went;
               let/ pos = go_cell(~null=(false, false), ~ind, ~pos, i, c);
               go_tok(~ind, ~pos, i, t);
             },
           );
      go_cell(~null=(false, true), ~ind, ~pos, List.length(ts), r);
    }
  and go_cell =
      (~null, ~ind, ~pos, i: int, c: Cell.t): Result.t(Path.Point.t, Pos.t) => {
    let c_end = Pos.add(pos, Dims.of_cell(c), ~return=ind.curr);
    Pos.lt(c_end, target)
      ? Error(c_end)
      : Pos.eq(c_end, target)
          ? Ok(Path.Point.(cons(i, mk(Idx.End(R)))))
          : go(~null, ~ind, ~pos, c) |> Result.map(~f=Path.Point.cons(i));
  }
  and go_tok =
      (~ind, ~pos, i: int, t: Token.t): Result.t(_, (Indented.t, Pos.t)) => {
    let t_end = Pos.add(pos, Dims.of_tok(t), ~return=ind.curr);
    let ind = Token.indent(t) ? Indented.indent_next(ind) : ind;
    Pos.lt(t_end, target)
      ? Error((ind, t_end))
      : Ok(Path.Point.mk(Tok(i, target.col - t_end.col)));
  };

  if (Pos.leq(target, Pos.zero)) {
    Path.Point.mk(End(L));
  } else {
    switch (go(c)) {
    | Ok(path) => path
    | Error(_) => Path.Point.mk(End(R))
    };
  };
};

module Range = {
  type t = (Pos.t, Pos.t);
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
