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

  let compare = (l, r) => {
    let c = Row.compare(l.row, r.row);
    c == 0 ? Col.compare(l.col, r.col) : c;
  };

  let eq = (l, r) => compare(l, r) == 0;
  let lt = (l, r) => compare(l, r) < 0;
  let leq = (l, r) => compare(l, r) <= 0;
};

module Indent_ctx = {
  type t = {
    // indentation of cell contents (if laid out vertically)
    body: Col.t,
    // indentation to return to at end of cell
    return: Col.t,
  };
  // let mk = (~curr=0, ~next=curr, ~end_=curr, ()) => {curr, next, end_};
  let init = col => {body: col, return: col};
  // let isize = 2;
  // let indent_next = ind => {...ind, next: ind.curr + isize};
};

module Traversed = {
  type t = {
    pos: Pos.t,
    ind: Col.t,
  };

  let init = {pos: Pos.zero, ind: 0};

  // let add = (~return: Col.t, p: t, dims: Dims.t) => {
  //   row: p.row + dims.height,
  //   col: (dims.height > 0 ? return : p.col) + dims.width,
  // };

  let add = (~return: Col.t, {pos, ind}: t, dims: Dims.t) =>
    if (dims.height <= 0) {
      let pos = {...pos, col: pos.col + dims.width};
      {ind, pos};
    } else {
      let ind = return;
      let pos = Pos.{row: pos.row + dims.height, col: return + dims.width};
      {ind, pos};
    };
};

// returns a valid path into c whose pos is nearest the given target,
// where nearest is defined by the ordering relation Pos.lt
let path_of_pos = (target: Pos.t, c: Cell.t): Path.Point.t => {
  open Result.Syntax;
  let rec go_cell =
          (~ctx: Indent_ctx.t, ~trav: Traversed.t, cell: Cell.t)
          : Result.t(Path.Point.t, Traversed.t) => {
    let c_end = Traversed.add(~return=ctx.return, trav, Dims.of_cell(cell));
    if (Pos.lt(c_end.pos, target)) {
      Error(c_end);
    } else if (Pos.eq(c_end.pos, target)) {
      Ok(Path.Point.mk(End(R)));
    } else {
      switch (Cell.get(cell)) {
      | None => Ok(Path.Point.here)
      | Some(m) => go_meld(~ctx, ~trav, m)
      };
    };
  }
  and go_meld =
      (~ctx: Indent_ctx.t, ~trav: Traversed.t, m: Meld.t)
      : Result.t(Path.Point.t, Traversed.t) => {
    Meld.to_chain(m)
    |> Chain.mapi_loop((step, cell) => (step, cell))
    |> Chain.fold_left(
         ((step, cell)) => {
           let ctx = {...ctx, return: ctx.body};
           go_cell(~ctx, ~trav, cell)
           |> Result.map(~f=Path.Point.cons(step));
         },
         (found, tok, (step, cell)) => {
           let/ trav = found;
           let/ trav =
             go_tok(~ctx, ~trav, tok)
             |> Result.map(~f=i => Path.Point.mk(Tok(step - 1, i)));
           let ctx =
             Indent_ctx.{
               body: trav.ind + (Token.indent(tok) ? 2 : 0),
               return: step == Meld.length(m) ? ctx.return : trav.ind,
             };
           go_cell(~ctx, ~trav, cell)
           |> Result.map(~f=Path.Point.cons(step));
         },
       );
  }
  and go_tok =
      (~ctx: Indent_ctx.t, ~trav: Traversed.t, tok: Token.t)
      : Result.t(int, Traversed.t) => {
    let t_end = Traversed.add(~return=ctx.return, trav, Dims.of_tok(tok));
    if (Pos.lt(t_end.pos, target)) {
      Error(t_end);
    } else {
      let rows = String.split_on_char('\n', tok.text);
      let n = List.length(rows);
      // num_chars: from start of token
      // origin: start column of row
      // len: length of row
      let (num_chars, origin, len) =
        rows
        |> List.fold_left_map(
             (num_chars, row) => {
               let len = Utf8.length(row);
               (num_chars + len, (num_chars, len));
             },
             0,
           )
        |> snd
        |> List.mapi((i, (num_chars, len)) => {
             let origin =
               i == 0 ? trav.pos.col : i < n - 1 ? ctx.body : ctx.return;
             (i, (num_chars, origin, len));
           })
        |> List.assoc(target.row - trav.pos.row);
      Ok(num_chars + min(max(0, target.col - origin), len));
    };
  };

  if (Pos.leq(target, Pos.zero)) {
    Path.Point.mk(End(L));
  } else {
    switch (go_cell(~ctx=Indent_ctx.init(0), ~trav=Traversed.init, c)) {
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
