let move = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  let+ ctx =
    switch (z.foc) {
    | Point =>
      let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
      // todo: add movement granularity
      switch (Token.pull(~from=b, tok)) {
      | None => Melder.Ctx.push_fail(~onto=b, tok, ctx)
      | Some((c, tok)) =>
        ctx
        |> Melder.Ctx.push_fail(~onto=d, tok)
        |> Melder.Ctx.push_fail(~onto=b, c)
      };
    | Select(_, sel) =>
      z.ctx
      |> Melder.Ctx.push_zigg(~onto=b, sel)
      |> Melder.Ctx.close
      |> Option.some
    };
  Zipper.mk(ctx);
};

let rec move_n = (n: int, z: Zipper.t): Zipper.t => {
  let move = (d, z) =>
    move(d, z) |> OptUtil.get_or_raise(Invalid_argument("Editor.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> move_n(n + 1)
  | _ when n > 0 => z |> move(R) |> move_n(n - 1)
  | _zero => z
  };
};
