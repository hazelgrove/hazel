open Sexplib.Std;
open Util;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Move.Action.t)
    | Select(Select.Action.t)
    | Delete(Dir.t)
    | Insert(string);
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
  |> Molder.remold(~fill=[Cell.Space.cursor])
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
  | Move(a) => Move.perform(a, z)
  | Select(a) => Select.perform(a, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
