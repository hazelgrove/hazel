open Sexplib.Std;
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
    let ctx =
      Melder.Ctx.push_zigg(~onto, sel, z.ctx)
      |> OptUtil.get_or_fail("bug: failed to push selection onto ctx");
    mk(ctx);
  };

let unzip = (cell: Cell.t) => {
  let get = OptUtil.get_or_raise(Path.Invalid);
  let rec go = (~ctx=Ctx.empty, cell: Cell.t) =>
    switch (cell.marks.cursor) {
    | None =>
      Ctx.map_fst(Frame.Open.cat(([], Melder.Slope.Up.unroll(cell))), ctx)
    | Some(({cells: [], token}, offset)) =>
      let M(l, w, r) = get(Cell.get(cell));
      let (pre, tok, suf) = Wald.unzip_tok(token, w);
      switch (Token.split(offset, tok)) {
      | Error(side) =>
        // normalize to overlapping cursor position in neighboring cell
        let n = Dir.pick(side, (token - 1, token));
        let path = Path.{cells: [n], token: 0};
        let marks = {...cell.marks, cursor: Some((path, 0))};
        go(~ctx, {...cell, marks});
      | Ok((tok_l, tok_r)) =>
        let l = Terr.{cell: l, wald: Wald.zip_tok(~suf=pre, tok_l)};
        let r = Terr.{wald: Wald.zip_tok(tok_r, ~suf), cell: r};
        Ctx.link((l, r), ctx);
      };
    | Some(({cells: [n, ..._], _}, _)) =>
      let M(l, w, r) = get(Cell.get(cell));
      if (n == 0) {
        let terr = Terr.{wald: w, cell: r};
        let ctx = Ctx.map_fst(Frame.Open.cat(([], [terr])), ctx);
        go(~ctx, l);
      } else if (n == Wald.length(w)) {
        let terr = Terr.{cell: l, wald: Wald.rev(w)};
        let ctx = Ctx.map_fst(Frame.Open.cat(([terr], [])), ctx);
        go(~ctx, r);
      } else {
        let (pre, cell, suf) = Wald.unzip_cell(n - 1, w);
        let terrs = Terr.({cell: l, wald: pre}, {wald: suf, cell: r});
        let ctx = Ctx.link(terrs, ctx);
        go(~ctx, cell);
      };
    };
  mk(go(cell));
};

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
       open_ => zip_open(open_, Cell.cursor),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );
