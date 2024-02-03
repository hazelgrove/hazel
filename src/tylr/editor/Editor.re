open Sexplib.Std;
open Util;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
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
    move(d, z) |> OptUtil.get_or_raise(Invalid_argument("Editor.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> move_n(n + 1)
  | _ when n > 0 => z |> move(R) |> move_n(n - 1)
  | _zero => z
  };
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
      let (tok, rest) = Melder.Zigg.pull(~from=d, zigg);
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

// d is side of cleared focus contents the cursor should end up
let clear_focus = (d: Dir.t, z: Zipper.t) =>
  switch (z.foc) {
  | Point => z.ctx
  | Select(_, sel) =>
    let onto = Dir.toggle(d);
    Melder.Ctx.push_zigg(~onto, sel, z.ctx);
  };

let pull_neighbors = ctx => {
  let pull = (from: Dir.t, ctx) =>
    switch (Melder.Ctx.pull(~from, ctx)) {
    | None => ("", ctx)
    | Some((tok, ctx)) =>
      Effects.remove(tok);
      (tok.text, ctx);
    };
  let (l, ctx) = pull(L, ctx);
  let (r, ctx) = pull(R, ctx);
  ((l, r), ctx);
};

let insert = (s: string, z: Zipper.t) => {
  let ctx = clear_focus(L, z);
  let ((l, r), ctx) = pull_neighbors(ctx);
  Labeler.label(l ++ s ++ r)
  |> List.fold_left((ctx, tok) => Molder.mold(ctx, tok), ctx)
  |> Molder.remold(~fill=[Cell.cursor])
  |> Zipper.mk
  |> Zipper.move_to_cursor;
};

let delete = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let+ z = Focus.is_empty(z.foc) ? select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
