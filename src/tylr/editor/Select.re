module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Un // unselect
    | All
    | Wald
    | Meld
    | Move(Move.Action.t);
};

let select = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  switch (z.foc) {
  | Point =>
    let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
    let foc = Focus.Select(d, Zigg.of_tok(tok));
    Zipper.mk(~foc, ctx);
  | Select(side, zigg) =>
    if (side == d) {
      let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
      let zigg = Melder.Zigg.grow(~side, tok, zigg);
      Zipper.mk(~foc=Select(d, zigg), ctx);
    } else {
      let (tok, rest) = Melder.Zigg.pull(~side=d, zigg);
      let ctx = Melder.Ctx.(close(push_fail(~onto=b, tok, z.ctx)));
      let foc =
        switch (rest) {
        | None => Focus.Point
        | Some(sel) => Select(b, sel)
        };
      Some(Zipper.mk(~foc, ctx));
    }
  };
};
