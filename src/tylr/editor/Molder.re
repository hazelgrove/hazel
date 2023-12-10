open Util;

let candidates = (t: EToken.Labeled.t): list(EToken.t) =>
  List.map(EToken.mk(~id=t.id, ~token=t.token),
    switch (t.mtrl) {
    | Space => [Space]
    | Grout(tips) => [Grout(tips)]
    | Tile([]) => [Tile(Unmolded(default_tips(p.token)))]
    | Tile(lbls) =>
      lbls
      |> List.concat_map(GMolds.with_label)
      |> List.map(m => GMtrl.Tile(Molded(m)))
    }
  );

let mold = (ctx: ECtx.t, ~cell=ECell.empty, t: EToken.Labeled.t) => {
  let molded =
    candidates(t)
    |> List.filter_map(t =>
      Melder.Ctx.push(~onto=L, t, ~cell, ctx)
    )
    |> List.stable_sort(((_, l), (_, r)) => Oblig.Delta.compare(l, r))
    |> ListUtil.hd_opt
    |> Option.map(fst);
  switch (molded) {
  | Some(ctx) => ctx
  | None =>
    let _ = failwith("todo: disassemble and remold cell");
    ctx
    |> Melder.Ctx.push(~onto=L, Labeler.unlabel(t))
    |> OptUtil.get_or_fail("bug: failed to meld unmolded token")
  };
};

let rec remold = (~cell=ECell.empty, ctx: ECtx.t) =>
  switch (ECtx.pull_terr(~from=R, ctx)) {
  | None =>
    ECtx.map_fst(EFrame.Open.cat((ESlope.Dn.unroll(slot), [])), ctx)
  | Some((terr, ctx)) =>
    let (face, rest) = EWald.split_face(~side=L, terr.wald);
    let molded = mold(ctx, ~cell, EToken.to_labeled(face));
    switch (ECtx.face(~side=L, molded)) {
    | Some(t) when t.mtrl == face.mtrl =>
      // fast path for when face piece retains mold
      molded
      |> ECtx.extend_face(~side=L, rest)
      |> remold(~cell=terr.cell)
    | _ =>
      // otherwise add rest of wald to suffix queue
      let up =
        switch (rest) {
        | ([], _) => []
        | ([cell, ...cells], ts) =>
          let terr = {...terr, wald: EWald.mk(ts, cells)};
          ESlope.Up.(cat(unroll(cell), [terr]));
        };
      ctx
      |> ECtx.map_fst(EFrame.Open.cat(([], up)))
      |> remold;
    }
  };

let rec remold = (~cell=ECell.empty, ctx: ECtx.t) =>
  switch (ECtx.pull)