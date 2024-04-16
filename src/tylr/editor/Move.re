open Util;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Step(Dir2.t)
    | Skip(Dir2.t)
    | Jump(Layout.Pos.t)
    | Hole(Dir.t);
};

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

let map_pos = (f: Layout.Pos.t => Layout.Pos.t, z: Zipper.t) => {
  let c = Zipper.zip(z);
  c.marks.cursor
  |> Option.map(path => {
       let pos = Layout.pos_of_path(path, c);
       let path = Layout.path_of_pos(f(pos), c);
       Zipper.unzip(Cell.put_cursor(path, c));
     })
  // shouldn't actually hit this case, just to type-check
  |> Option.value(~default=z);
};

// todo: need to return none in some more cases when no visible movement occurs
let perform = (a: Action.t, z: Zipper.t) =>
  switch (a) {
  | Step(H(d)) => move(d, z)
  | Step(V(d)) =>
    // todo: this always succeeds but should fail at ends
    z
    |> map_pos(pos => {...pos, row: pos.row + Dir.pick(d, ((-1), 1))})
    |> Option.some
  | Skip(H(d)) =>
    z
    |> map_pos(pos => {...pos, col: Dir.pick(d, ((-1), Int.max_int))})
    |> Option.some
  | Skip(V(d)) =>
    let c = Zipper.zip(z);
    let path = Path.Point.mk(End(d));
    Some(Zipper.unzip(Cell.put_cursor(path, c)));
  | Jump(pos) => z |> map_pos(_ => pos) |> Option.some
  | Hole(_) => failwith("todo: move to hole")
  };
