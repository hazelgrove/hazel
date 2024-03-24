open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Focus.t,
  ctx: Ctx.t,
};

let mk = (~foc=Focus.Point, ctx) => {foc, ctx};

let unselect = (~toward=?, z: t) =>
  switch (z.foc) {
  | Point => z
  | Select(d, sel) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    mk(Melder.Ctx.push_zigg(~onto, sel, z.ctx));
  };

let unroll_cell = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L
      ? ([], Melder.Slope.Up.unroll(cell))
      : (Melder.Slope.Dn.unroll(cell), []);
  Ctx.map_fst(Frame.Open.cat(f_open), ctx);
};
let unzip_cell =
    (~ctx=Ctx.empty, n: Path.Cell.Idx.t, cell: Cell.t): (Cell.t, Ctx.t) => {
  let M(l, w, r) = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
  if (n == 0) {
    let terr = Terr.{wald: w, cell: r};
    let ctx = Ctx.map_fst(Frame.Open.cat(([], [terr])), ctx);
    (l, ctx);
  } else if (n == Wald.length(w)) {
    let terr = Terr.{cell: l, wald: Wald.rev(w)};
    let ctx = Ctx.map_fst(Frame.Open.cat(([terr], [])), ctx);
    (r, ctx);
  } else {
    let (pre, cell, suf) = Wald.unzip_cell(n - 1, w);
    let terrs = Terr.({cell: l, wald: pre}, {wald: suf, cell: r});
    (cell, Ctx.link(terrs, ctx));
  };
};
let rec unzip_point = (~ctx=Ctx.empty, p: Path.Point.Idx.t, cell: Cell.t) =>
  switch (p) {
  | End(side) => unroll_cell(~ctx, side, cell)
  | Tok(i, j) =>
    let M(l, w, r) = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
    let (pre, tok, suf) = Wald.unzip_tok(i, w);
    switch (Token.split(j, tok)) {
    | Error(side) =>
      // normalize to overlapping cursor position in neighboring cell
      let n = Dir.pick(side, (i - 1, i));
      let (cell, ctx) = unzip_cell(~ctx, n, cell);
      unzip_point(~ctx, End(Dir.toggle(side)), cell);
    | Ok((tok_l, tok_r)) =>
      let l = Terr.{cell: l, wald: Wald.zip_tok(~suf=pre, tok_l)};
      let r = Terr.{wald: Wald.zip_tok(tok_r, ~suf), cell: r};
      Ctx.link((l, r), ctx);
    };
  };
let unzip_cursor = (~ctx=Ctx.empty, cell: Cell.t) =>
  switch (cell.marks.cursor) {
  | None => unroll_cell(L, cell, ~ctx)
  | Some((cells, point)) =>
    let (cell, ctx) =
      cells
      |> List.fold_left(
           ((cell, ctx), n) => unzip_cell(n, cell, ~ctx),
           (cell, ctx),
         );
    unzip_point(point, cell, ~ctx);
  };
let unzip = (cell: Cell.t) => mk(unzip_cursor(cell));

let zip_closed = ((l, r): Frame.Closed.t, zipped: Cell.t) => {
  let w = Wald.zip_cell(l.wald, zipped, r.wald);
  Cell.put(Meld.mk(~l=l.cell, w, ~r=r.cell));
};
let rec zip_open = ((dn, up): Frame.Open.t, zipped: Cell.t) =>
  switch (dn, up) {
  | ([], []) => zipped
  | ([], [_, ..._]) => Melder.Slope.Up.roll(~init=zipped, up)
  | ([_, ..._], []) => Melder.Slope.Dn.roll(dn, ~init=zipped)
  | ([l, ..._] as dn, [r, ..._]) when Melder.Wald.lt(l.wald, r.wald) =>
    Cell.put(Meld.mk(~l=zipped, r.wald, ~r=r.cell)) |> zip_open((dn, up))
  | ([l, ...dn], [r, ..._] as up) when Melder.Wald.gt(l.wald, r.wald) =>
    Cell.put(Meld.mk(~l=l.cell, l.wald, ~r=zipped)) |> zip_open((dn, up))
  | ([l, ...dn], [r, ...up]) =>
    zipped |> zip_closed((l, r)) |> zip_open((dn, up))
  };
let zip = (z: t) =>
  z.ctx
  |> Ctx.fold(
       open_ => zip_open(open_, Meld.cursor),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );

let move_to_cursor = (z: t) => unzip(zip(z));
