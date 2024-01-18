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
  let map = failwith("todo");

  let with_label = lbl =>
    switch (find_opt(lbl, map)) {
    | None => []
    | Some(ms) => ms
    };
};

let candidates = (t: Token.Labeled.t): list(Token.t) =>
  List.map(
    Token.mk(~id=t.id, ~token=t.token),
    switch (t.mtrl) {
    | Space => [Molded.Label.space]
    | Grout => failwith("bug: attempted to mold grout")
    | Tile(lbls) =>
      lbls
      |> List.concat_map(lbl =>
           Molds.with_label(lbl)
           |> List.map(mold => Molded.{mold, mtrl: Tile(lbl)})
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
    |> Melder.Ctx.push(~onto=L, Token.unlabel(t))
    |> OptUtil.get_or_fail("bug: failed to meld unmolded token")
  };
};

let rec remold = (~fill=[], ctx: Ctx.t) =>
  switch (Ctx.pull_terr(~from=R, ctx)) {
  | None =>
    let unrolled = fill |> List.rev_map(Slope.Dn.unroll_meld) |> List.concat;
    Ctx.map_fst(Frame.Open.cat((unrolled, [])), ctx);
  | Some((terr, ctx)) when Mtrl.is_grout(Terr.sort(terr)) =>
    let up = Terr.cells(terr) |> List.concat_map(Slope.Up.unroll_cell);
    ctx |> Ctx.map_fst(Frame.Open.cat(([], up))) |> remold(~fill);
  | Some((terr, ctx)) =>
    let (face, rest) = Wald.split_face(terr.wald);
    let molded = mold(ctx, ~fill, Token.to_labeled(face));
    switch (Ctx.face(~side=L, molded)) {
    | Some(t) when t.mtrl == face.mtrl =>
      // fast path for when face piece retains mold
      let _ = failwith("todo: make sure cell distributes paths");
      molded
      |> Ctx.extend_face(~side=L, rest)
      |> remold(~fill=Option.to_list(cell.meld));
    | _ =>
      // otherwise add rest of wald to suffix queue
      let up =
        switch (rest) {
        | ([], _) => []
        | ([cell, ...cells], toks) =>
          let terr = {...terr, wald: Wald.mk(toks, cells)};
          let _ = failwith("todo: make sure cell distributes paths");
          Slope.Up.(cat(unroll(Option.to_list(cell)), [terr]));
        };
      ctx |> Ctx.map_fst(Frame.Open.cat(([], up))) |> remold;
    };
  };
