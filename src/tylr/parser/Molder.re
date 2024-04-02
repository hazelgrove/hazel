open Util;

// replace ghost with piece above bridge
// let x = 1 >in< x + 1
// let x = 1 >in< x + 1 [in]
// let x = 1 >< x + 1 [in]
// let x = 1 >< x + 1 in <>

// replace ghost with piece under bridge
// let x = 1 + 2 >in< x + 1
// let x = 1 [in] + 2 >in< x + 1
//
// let x = 1 in <> + 2 >< x + 1

// replacing even solid bridges?
// let x = 1 + 2 in x + 1
// let x = 1 [in] + 2 in x + 1
//
// let x = 1 in <> + 2 >in< x + 1
// or
// let x = 1 in <> + 2 >< <in> >< x + 1

module Molds = {
  include Mtrl.Labeled.Map;
  type t = Mtrl.Labeled.Map.t(list(Mold.t));
  let map: t =
    Walker.walk_into(~from=L, Root)
    |> Walk.Index.to_list
    |> List.rev_map(fst)
    |> List.fold_left(
         map =>
           fun
           | Bound.Root => map
           | Node(Molded.{mtrl, mold}) =>
             map
             |> update(
                  mtrl,
                  fun
                  | None => Some([mold])
                  | Some(ms) => Some([mold, ...ms]),
                ),
         empty,
       );

  let with_label = lbl =>
    switch (find_opt(lbl, map)) {
    | None => []
    | Some(ms) => ms
    };
};

let candidates = (t: Token.Labeled.t): list(Token.t) =>
  List.map(
    Token.mk(~id=t.id, ~text=t.text),
    switch (t.lbl) {
    | Space => [Molded.Labeled.space]
    | Grout => failwith("bug: attempted to mold grout")
    | Tile(lbls) =>
      lbls
      |> List.concat_map(lbl =>
           Molds.with_label(Tile(lbl))
           |> List.map(mold => Molded.{mold, mtrl: Mtrl.Tile(lbl)})
         )
    },
  );

let mold = (ctx: Ctx.t, ~fill=[], t: Token.Labeled.t) => {
  switch (
    candidates(t)
    |> Oblig.Delta.minimize(tok => Melder.Ctx.push(~onto=L, tok, ~fill, ctx))
  ) {
  | Some(ctx) => ctx
  | None =>
    ctx
    |> Melder.Ctx.push(~onto=L, Token.Labeled.unlabel(t))
    |> OptUtil.get_or_fail("bug: failed to meld unmolded token")
  };
};

let rec remold = (~fill=[], ctx: Ctx.t) => {
  let ((dn, up), tl) = Ctx.split_fst(ctx);
  switch (up) {
  | [] =>
    let unrolled =
      fill |> List.rev_map(Melder.Slope.Dn.unroll) |> List.concat;
    Ctx.zip((Slope.cat(unrolled, dn), []), ~suf=tl);
  | [terr, ...up] when Terr.sort(terr) == Mtrl.Grout =>
    let unrolled =
      Terr.cells(terr) |> List.concat_map(Melder.Slope.Up.unroll);
    remold(Ctx.zip((dn, Slope.cat(unrolled, up)), ~suf=tl));
  | [terr, ...up] =>
    let ctx = Ctx.put_fst((dn, up), ctx);
    let (hd, rest) = Wald.split_hd(terr.wald);
    let molded = mold(ctx, ~fill, Token.to_labeled(hd));
    switch (Ctx.face(~side=L, molded)) {
    | Some(lbl) when lbl == hd.lbl =>
      // fast path for when face piece retains mold
      molded
      |> Ctx.extend(~side=L, rest)
      |> Option.get  // must succeed if Ctx.face succeeded
      |> remold(~fill=Baked.Fill.init(terr.cell))
    | _ =>
      // otherwise add rest of wald to suffix queue
      let up =
        switch (rest) {
        | ([], _) => []
        | ([cell, ...cells], toks) =>
          let terr = {...terr, wald: Wald.mk(toks, cells)};
          let _ = failwith("todo: make sure cell distributes paths");
          Slope.cat(Melder.Slope.Up.unroll(cell), [terr]);
        };
      ctx |> Ctx.map_fst(Frame.Open.cat(([], up))) |> remold;
    };
  };
};