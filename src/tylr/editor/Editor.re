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
  let b = Dir.toggle(d);
  switch (z.foc) {
  | Point =>
    open OptUtil.Syntax;
    let+ (t, ctx) = Ctx.pull(~from=d, z.ctx);
    let n = Dir.choose(d, Token.length(t) - 1, 1);
    switch (Token.unzip(n, p)) {
    | None => ctx |> Melder.Ctx.push(~onto=b, t) |> Zipper.mk
    | Some((l, r)) =>
      ctx
      |> Melder.Ctx.push(~onto=d, Dir.choose(d, l, r))
      |> Melder.Ctx.push(~onto=b, Dir.choose(b, l, r))
      |> Zipper.mk
    };
  | Select(_, sel) =>
    z.ctx
    |> Melder.Ctx.push_zigg(~onto=b, sel)
    |> Melder.Ctx.close
    |> Zipper.mk
    |> Option.some
  };
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
  let sel = Zipper.selected(z);
  switch (d, z.foc) {
  | (_, Point)
  | (L, Select(L, _))
  | (R, Select(R, _)) =>
    open OptUtil.Syntax;
    let+ (t, ctx) = Ctx.pull(~from=d, z.ctx);
    let sel = Melder.Zigg.push(~onto=d, t, sel);
    Zipper.mk(~foc=Select(d, sel), ctx);
  | (L, Select(R as b, _))
  | (R, Select(L as b, _)) =>
    let (t, rest) = EZigg.pull(~from=d, sel);
    let foc =
      switch (rest) {
      | None => Zipper.Focus.Point
      | Some(sel) => Select(b, sel)
      };
    z.ctx
    |> Melder.Ctx.push(~onto=b, t)
    |> Zipper.mk(~close=true, ~foc)
    |> Option.some;
  };
};

let save_cursor = Melder.Ctx.push_or_fail(~onto=L, Token.mk_cursor());
let load_cursor = ctx => ctx |> Zipper.zip |> Zipper.unzip;

// d is side of cleared focus contents the cursor should end up
let clear_focus = (d: Dir.t, z: Zipper.t) =>
  switch (z.foc) {
  | Point => z.ctx
  | Select(_, sel) =>
    let onto = Dir.toggle(d);
    Melder.Ctx.push_zigg(~onto, sel, z.ctx);
  };

let insert = (s: string, z: Zipper.t) => {
  let ctx = clear_focus(L, z);
  let (l, ctx) = Ctx.pull(~from=L, ctx);
  let (r, ctx) = Ctx.pull(~from=R, ctx);
  Labeler.label(l ++ s ++ r)
  |> List.fold_left(Molder.mold, ctx)
  |> save_cursor
  |> Molder.remold
  |> load_cursor
  |> Zipper.mk;
};

let delete = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let+ z = Zipper.Focus.is_empty(z.foc) ? select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
