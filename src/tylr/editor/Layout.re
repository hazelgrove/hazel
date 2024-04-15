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

  let skip_col = (n, pos) => {...pos, col: pos.col + n};

  let skip = (pos: t, ~over: Dims.t, ~return: Col.t) => {
    let h = Dims.Height.total(over.height);
    let w = Dims.Width.total(over.width);
    {row: pos.row + h, col: (h > 0 ? return : pos.col) + w};
  };
};

module Ictx = {
  type t = {
    // left delimiter
    delim: Bound.t(Token.t),
    // indentation at start of cell (before any newlines)
    left: Col.t,
    // indentation to return to at end of cell
    right: Col.t,
  };
  let init = {delim: Root, left: 0, right: 0};
  let middle = (~newline=true, ctx: t) => {
    let indent =
      ctx.delim |> Bound.map(Token.indent) |> Bound.get(~root=false);
    ctx.left + (indent && newline ? 2 : 0);
  };
};

let pos_of_path = (path: Path.Point.t, cell: Cell.t): Pos.t => {
  let rec go = (~ctx: Ictx.t, ~pos: Pos.t, path: Path.Point.t, cell) => {
    let dims = Dims.of_cell(cell);
    let (l, r) = (pos, Pos.skip(pos, ~over=dims, ~return=ctx.right));
    switch (Cell.Space.get(cell)) {
    | Some(spc) =>
      switch (path) {
      | ([0], _)
      | ([], End(L)) => l
      | ([1], _)
      | ([], End(R)) => r
      | ([], Tok(0, j)) =>
        let mid = Ictx.middle(~newline=dims.height.head > 0, ctx);
        let (l, r) = Utf8.split(j, spc.text);
        let h_r = List.length(String.split_on_char('\n', r)) - 1;
        Pos.skip(
          pos,
          ~over=Dims.of_space(l),
          ~return=h_r > 0 ? mid : ctx.right,
        );
      | _ => raise(Path.Invalid)
      }
    | None =>
      let m = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
      switch (path) {
      | ([], End(L)) => l
      | ([], End(R)) => r
      | ([], Tok(i, j)) =>
        let (pre, _, _) = Meld.unzip_tok(i, m);
        let mid = Ictx.middle(~newline=dims.height.head > 0, ctx);
        pre
        |> Chain.fold_left(
             cell => Pos.skip(pos, ~over=Dims.of_cell(cell), ~return=mid),
             (pos, tok, cell) =>
               pos
               |> Pos.skip(~over=Dims.of_tok(tok), ~return=mid)
               |> Pos.skip(~over=Dims.of_cell(cell), ~return=mid),
           )
        |> Pos.skip_col(j);
      | ([c, ...cs], p) =>
        let m = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
        let mid = Ictx.middle(~newline=Dims.of_meld(m).height.head > 0, ctx);
        let ((toks, cells), cell, suf) = Meld.unzip(c, m);
        let pos =
          List.fold_right2(
            (tok, cell, pos) =>
              pos
              |> Pos.skip(~over=Dims.of_cell(cell), ~return=mid)
              |> Pos.skip_col(Token.length(tok)),
            toks,
            cells,
            pos,
          );
        let ctx =
          switch (suf) {
          | ([], _) => {...ctx, right: mid}
          | ([t, ..._], _) => {...ctx, delim: Node(t), left: mid}
          };
        go(~pos, ~ctx, (cs, p), cell);
      };
    };
  };
  go(~ctx=Ictx.init, ~pos=Pos.zero, path, cell);
};

// returns a valid path into c whose pos is nearest the given target,
// where nearest is defined by the ordering relation Pos.lt
let path_of_pos = (target: Pos.t, c: Cell.t): Path.Point.t => {
  open Result.Syntax;
  let rec go_cell =
          (~ctx: Ictx.t, ~pos: Pos.t, cell: Cell.t)
          : Result.t(Path.Point.t, Pos.t) => {
    let c_end = Pos.skip(pos, ~over=Dims.of_cell(cell), ~return=ctx.right);
    if (Pos.lt(c_end, target)) {
      Error(c_end);
    } else if (Pos.eq(c_end, target)) {
      Ok(Path.Point.mk(End(R)));
    } else {
      switch (Cell.get(cell)) {
      | None => Ok(Path.Point.here)
      | Some(m) => go_meld(~ctx, ~pos, m)
      };
    };
  }
  and go_meld =
      (~ctx: Ictx.t, ~pos: Pos.t, m: Meld.t): Result.t(Path.Point.t, Pos.t) => {
    let dims = Dims.of_meld(m);
    // indentation of meld's root tokens
    let ind = Ictx.middle(~newline=dims.height.head > 0, ctx);
    Meld.to_chain(m)
    |> Chain.mapi_loop((step, cell) => (step, cell))
    |> Chain.fold_left(
         ((step, cell)) => {
           go_cell(~ctx={...ctx, right: ind}, ~pos, cell)
           |> Result.map(~f=Path.Point.cons(step))
         },
         (found, tok, (step, cell)) => {
           let/ pos = found;
           let/ pos =
             go_tok(~ctx, ~pos, tok)
             |> Result.map(~f=i => Path.Point.mk(Tok(step - 1, i)));
           let ctx =
             Ictx.{
               delim: Node(tok),
               left: ind,
               right: step == Meld.length(m) ? ctx.right : ind,
             };
           go_cell(~ctx, ~pos, cell) |> Result.map(~f=Path.Point.cons(step));
         },
       );
  }
  // ctx is ctx of containing meld
  and go_tok =
      (~ctx: Ictx.t, ~pos: Pos.t, tok: Token.t): Result.t(int, Pos.t) => {
    let _ = failwith("using ctx.right below doesn't always seem right...");
    let t_end = Pos.skip(pos, ~over=Dims.of_tok(tok), ~return=ctx.right);
    if (Pos.lt(t_end, target)) {
      Error(t_end);
    } else {
      let lines = String.split_on_char('\n', tok.text);
      let n = List.length(lines);
      lines
      |> List.mapi((i, line) => (i, line))
      |> List.fold_left(
           (found, (i, line)) => {
             let/ (chars, pos: Pos.t) = found;
             // count newline
             let chars = i == 0 ? chars : chars + 1;
             let row = i == 0 ? pos.row : pos.row + 1;
             // start of row
             let col =
               i == 0 ? ctx.left : i == n - 1 ? ctx.right : Ictx.middle(ctx);
             let len = Utf8.length(line);
             if (row < target.row) {
               Error((chars + len, Pos.{row, col: col + len}));
             } else if (col + len < target.col) {
               i == n - 1
                 ? Error((chars + len, Pos.{row, col: col + len}))
                 : Ok(chars + len);
             } else {
               Ok(chars + target.col - col);
             };
           },
           Error((0, pos)),
         )
      |> Result.map_error(~f=snd);
    };
  };

  if (Pos.leq(target, Pos.zero)) {
    Path.Point.mk(End(L));
  } else {
    switch (go_cell(~ctx=Ictx.init, ~pos=Pos.zero, c)) {
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
