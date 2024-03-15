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
    move(d, z) |> OptUtil.get_or_raise(Invalid_argument("Move.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> move_n(n + 1)
  | _ when n > 0 => z |> move(R) |> move_n(n - 1)
  | _zero => z
  };
};

let map_pos = (f: Measured.Pos.t => Measured.Pos.t, z: Zipper.t) => {
  let (c, path) = Zipper.zip(z);
  let pos = Measured.pos_of_path(path, c);
  let path = Measured.path_of_pos(f(pos));
  Zipper.unzip(Cell.put_cursor(path, c));
};

let go = (a: Action.Move.t, z: Zipper.t) =>
  switch (a) {
  | Step(H(d)) => move(d, z)
  | Step(V(d)) =>
    z |> map_pos(pos => {...pos, row: pos.row + Dir.pick(d, -1, 1)})
  | Skip(H(d)) =>
    z |> map_pos(pos => {...pos, col: Dir.pick(d, -1, Int.max_int)})
  | Skip(V(d)) =>
    let (c, _) = Zipper.zip(z);
    let path = Path.Point.mk(End(d));
    Zipper.unzip(Cell.put_cursor(path, c));
  | Jump(pos) => z |> map_pos(_ => pos)
  | Hole(_) => failwith("todo: move to hole")
  };
