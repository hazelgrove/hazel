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
      switch (Token.unzip(offset, tok)) {
      | Error(side) =>
        // normalize to overlapping cursor position in neighboring cell
        let n = Dir.pick(side, (token - 1, token));
        let path = Path.{cells: [n], token: 0};
        let marks = {...cell.marks, cursor: Some((path, 0))};
        go(~ctx, {...cell, marks});
      | Ok((tok_l, tok_r)) =>
        let l = Terr.{cell: l, wald: Wald.zip(~suf=pre, tok_l)};
        let r = Terr.{wald: Wald.zip(tok_r, ~suf), cell: r};
        // should this become a closed frame?
        Ctx.map_fst(Frame.Open.cat(([l], [r])), ctx);
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

let zip_up = (_, _) => failwith("todo");
let zip_dn = (_, _) => failwith("todo");
let rec zip_slopes = ((dn, up): Slopes.t, z: Zipped.t) =>
  switch (dn, up) {
  | ([], []) => z
  | ([], [_, ..._] as up) => Slope.Up.zip(z, up)
  | ([_, ..._] as dn, []) => Slope.Dn.zip(dn, z)
  | ([hd_dn, ...tl_dn], [hd_up, ...tl_up]) =>
    let (p, slot) = z;
    let check = b => b ? () : raise(Invalid_argument("Slopes.zip"));
    // expecting no error-correction to take place
    switch (Melder.Wald.cmp(hd_dn.wal, ~slot, hd_up.wal)) {
    | {up: [], dn: [], top} =>
      // eq
      let p = Path.cons(Wald.length(hd_dn.wal), p);
      let slot = Slot.full((hd_dn.mel, top, hd_up.mel));
      zip_slopes((tl_dn, tl_up), (p, slot));
    | {up: [], dn: [hd, ...tl], _} =>
      // lt
      check(tl == []);
      let p = Path.cons(0, p);
      let slot = Slot.full((hd.mel, hd.wal, hd_up.mel));
      zip_slopes((dn, tl_up), slot);
    | {up: [hd, ...tl], _} =>
      // gt
      check(tl == []);
      let p = Path.cons(Wald.length(hd_dn.wal), p);
      let slot = Slot.full((hd_dn.mel, hd.wal, hd.mel));
      zip_slopes((tl_dn, up), slot);
    };
  };
let zip = (z: t): Zipped.t =>
  unselect(z).ctx |> Stepwell.break_bridges |> zip_slopes;

// todo: cleanup
// let push_sel = (lx: Lexeme.t, foc: Dir.t, sel) =>
//   switch (foc) {
//   | L => Result.unwrap(Ziggurat.push_lexeme(lx, sel))
//   | R => Result.unwrap(Ziggurat.hsup_lexeme(sel, lx))
//   };
// let pull_sel = (~char=false, foc: Dir.t, sel) =>
//   switch (foc) {
//   | L => Ziggurat.pull_lexeme(~char, sel)
//   | R => Ziggurat.llup_lexeme(~char, sel) |> Option.map(((a, b)) => (b, a))
//   };
