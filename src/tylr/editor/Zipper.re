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

let unroll_cell = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L
      ? ([], Melder.Slope.Up.unroll(cell))
      : (Melder.Slope.Dn.unroll(cell), []);
  Ctx.map_fst(Frame.Open.cat(f_open), ctx);
};

let unzip_to_cell = (~ctx=Ctx.empty, n: Path.Cell.Idx.t, cell: Cell.t): (Cell.t, Ctx.t) => {
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

let rec unzip_to_point = (~ctx=Ctx.empty, p: Path.Point.Idx.t, cell: Cell.t) =>
  switch (p) {
  | End(side) => unroll_cell(~ctx, side, cell)
  | Tok(i, j) =>
    let M(l, w, r) = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
    let (pre, tok, suf) = Wald.unzip_tok(i, w);
    switch (Token.split(j, tok)) {
    | Error(side) =>
      // normalize to overlapping cursor position in neighboring cell
      let n = Dir.pick(side, (i - 1, i));
      let (cell, ctx) = unzip_to_cell(~ctx, n, cell);
      unzip_to_point(~ctx, End(Dir.toggle(side)), cell);
    | Ok((tok_l, tok_r)) =>
      let l = Terr.{cell: l, wald: Wald.zip_tok(~suf=pre, tok_l)};
      let r = Terr.{wald: Wald.zip_tok(tok_r, ~suf), cell: r};
      Ctx.link((l, r), ctx);
    };
  };

let rec unzip = (~ctx=Ctx.empty, cell: Cell.t) =>
  switch (cell.marks.cursor) {
  | None => mk(unroll_cell(L, cell, ~ctx))
  | Some((side, range)) =>
    let foc =
      fun
      | None => Focus.Point
      | Some(zigg) => Select(side, zigg);
    switch (Path.Range.uncons(range)) {
    | Some((n, _)) =>
      let (cell, ctx) = unzip_to_cell(n, cell, ~ctx);
      unzip(cell, ~ctx);
    | None =>
      switch (range) {
      // | (p, q) when p == q =>
      //   let (ns, p) = p;
      //   let (cell, ctx) =
      //     ns
      //     |> List.fold_left(
      //       ((cell, ctx), n) => unzip_to_cell(n, cell, ~ctx),
      //       (cell, ctx),
      //     );
      //   mk(unzip_to_point(p, cell, ~ctx));
      | (([], End(_)), q) =>
        let marks_q = {...cell.marks, cursor: Some(Path.Range.point(q))};
        let cell_q = {...cell, marks: marks_q};
        let (dn, up) = Melder.Ctx.open_(unzip(cell_q));
        ctx
        |> Ctx.map_fst(Frame.Open.cat(([], up)))
        |> mk(~foc=foc(Melder.Zigg.of_dn(dn)));
      | (p, ([], End(_))) =>
        let marks_p = {...cell.marks, cursor: Some(Path.Range.point(p))};
        let cell_p = {...cell, marks: marks_p};
        let (dn, up) = Melder.Ctx.open_(unzip(cell_p));
        ctx
        |> Ctx.map_fst(Frame.Open.cat((dn, [])))
        |> mk(~foc=foc(Melder.Zigg.of_up(up)));
      | (([], Tok(i_p, j_p)), ([], Tok(i_q, j_q))) when i_p == i_q =>
        let M(l, w, r) =
          Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
        let (pre, tok, suf) = Wald.unzip_tok(i_p, w);
        switch (Token.unzip_range((j_p, j_q), tok)) {
        | Error(side) =>
          let n = Dir.pick(side, (i_p, i_p + 1));
          let p: Path.Point.t = ([n], End(Dir.toggle(side)));
          let marks = {...cell.marks, cursor: Some(Path.Range.point(p))};
          let cell = {...cell, marks};
          unzip(~ctx, cell);
        | Ok(Point(tok_l, tok_r)) =>
          let l = Terr.{cell: l, wald: Wald.zip(~suf=pre, tok_l)};
          let r = Terr.{wald: Wald.zip(tok_r, ~suf), cell: r};
          mk(Ctx.link((l, r), ctx));
        | Ok(Select(tok_l, tok_m, tok_r)) =>
          let dn =
            switch (tok_l) {
            | Some(tok_l) => [Terr.{wald: Wald.zip(tok_l, ~suf=pre), cell: l}]
            | None =>
              switch (pre) {
              | ([], _) => []
              | ([c, ...cs], ts) =>
                Melder.Slope.Dn.unroll(c) @ [Terr.{wald: Wald.mk(ts, cs), cell: l}]
              }
            };
          let up =
            switch (tok_r) {
            | Some(tok_r) => [Terr.{wald: Wald.zip(tok_r, ~suf), cell: r}]
            | None =>
              switch (suf) {
              | ([], _) => []
              | ([c, ...cs], ts) =>
                Melder.Slope.Up.unroll(c) @ [Terr.{wald: Wald.mk(ts, cs), cell: r}]
              }
            };
          ctx
          |> Ctx.map_fst(Frame.Open.cat((dn, up)))
          |> Zipper.mk(~foc=foc(Zigg.of_tok(tok_m)));
        };
      | (([], Tok(i_p, j_p)), ([], Tok(i_q, j_q))) =>
        let M(l, W(toks, mids), r) =
          Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
        let (ts_pre, t_p, ts_mid, t_q, ts_suf) = ListUtil.split_two(i_p, i_q, toks);
        let (cs_pre, cs_mid, cs_suf) = ListUtil.split_sub((i_p, i_q), mids);
        ?

        let (t_p, (ts_pre, ts_suf)) = ListUtil.split_frame(i_p, toks);
        let (t_q, (ts_mid, ts_suf)) = ListUtil.split_frame(i_q - i_p - 1, ts_suf);


      | (([], Tok(_) as p), ([], End(_))) =>
        let (dn, up) = Melder.Ctx.open_(unzip_to_point(p, cell));
        ctx
        |> Ctx.map_fst(Frame.Open.cat((dn, [])))
        |> mk(~foc=foc(Melder.Zigg.of_up(up)));

      | _ => x
      };
    }
  };


let rec unzip_cursor = (~ctx, cursor: Path.Cursor.t, cell: Cell.t) => {
  switch (cursor) {
  | None => unroll_cell(~ctx, L, cell)
  | Some((side, range)) =>
    switch (range) {
    | (([], End(l)), ([], End(r))) when l == r => unroll(l)
    | (([], End(_)), ([], End(_))) =>
      switch (Melder.Zigg.unroll(cell)) {
      | None => mk(ctx)
      | Some(zigg) => mk(~foc=Select(side, zigg), ctx)
      }
    | (([], End(l)), ([], Tok(i, j))) =>
      let M(l, w, r) = get(Cell.get(cell));
      let (pre, tok, suf) = Wald.unzip_tok(i, w);
      switch (Token.split(j, tok)) {
      | Error(side) =>
        // normalize to overlapping cursor position in neighboring cell
        let n = Dir.pick(side, (i - 1, i));
        let l: Path.Point.t = ([n], End(Dir.toggle(side)));
        let r: Path.Point.t = ([], Tok(i, j));
        let marks = {...cell.marks, cursor: Some((l, r))};
        go(~ctx, (l, r), {...cell, marks});
      | Ok((tok_l, tok_r)) =>
        let l = Terr.{cell: l, wald: Wald.zip_tok(~suf=pre, tok_l)};
        let r = Terr.{wald: Wald.zip_tok(tok_r, ~suf), cell: r};
        Ctx.link((l, r), ctx);
      };
    | _ => x
    }
  };
};

let unzip = (cell: Cell.t) => {
  let get = OptUtil.get_or_raise(Path.Invalid);
  let rec go = (~ctx=Ctx.empty, cell: Cell.t) =>
    switch (cell.marks.cursor) {
    | None =>
      // default cursor position at left end
      Ctx.map_fst(Frame.Open.cat(([], Melder.Slope.Up.unroll(cell))), ctx)
    | Some(range) =>
      switch (Path.Range.uncons(range)) {
      | None when Path.Range.is_point(range) =>
        let (_empty, p) = Path.Range.origin(range);
        switch (p) {
        | End(side) =>
          let slopes =
            switch (side) {
            | L => ([], Melder.Slope.Up.unroll(cell))
            | R => (Melder.Slope.Dn.unroll(cell), [])
            };
          Ctx.map_fst(Frame.Open.cat(slopes), ctx);
        | Tok(i, j) =>
          let M(l, w, r) = get(Cell.get(cell));
          let (pre, tok, suf) = Wald.unzip_tok(i, w);
          switch (Token.split(j, tok)) {
          | Error(side) =>
            // normalize to overlapping cursor position in neighboring cell
            let n = Dir.pick(side, (i - 1, i));
            let p: Path.Point.t = ([n], End(Dir.toggle(side)));
            let marks = {...cell.marks, cursor: Some(Path.Range.point(p))};
            go(~ctx, {...cell, marks});
          | Ok((tok_l, tok_r)) =>
            let l = Terr.{cell: l, wald: Wald.zip_tok(~suf=pre, tok_l)};
            let r = Terr.{wald: Wald.zip_tok(tok_r, ~suf), cell: r};
            Ctx.link((l, r), ctx);
          };
        };
      | None => x
      | Some((n, _)) =>
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
      }
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
