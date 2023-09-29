module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: EZipper.t): option(EZipper.t) => {
  let b = Dir.toggle(d);
  switch (z.foc) {
  | Pointing =>
    open OptUtil.Syntax;
    let+ (p, ctx) = EStepwell.pull(~from=d, z.ctx);
    let n = Dir.choose(d, Piece.length(p) - 1, 1);
    switch (Piece.unzip(n, p)) {
    | Some((l, r)) =>
      ctx
      |> Melder.Stepwell.push_piece(~onto=d, Dir.choose(d, l, r))
      |> Melder.Stepwell.push_piece(~onto=b, Dir.choose(b, l, r))
      |> EZipper.mk
    | None => ctx |> Melder.Stepwell.push_piece(~onto=b, p) |> EZipper.mk
    };
  | Selecting(_, sel) =>
    z.ctx
    |> Melder.Stepwell.push_zigg(~onto=b, sel)
    |> Melder.Stepwell.rebridge
    |> EZipper.mk
    |> Option.some
  };
};

let rec move_n = (n: int, z: EZipper.t): EZipper.t => {
  let move = (d, z) =>
    move(d, z) |> OptUtil.get_or_raise(Invalid_argument("Editor.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> move_n(n + 1)
  | _ when n > 0 => z |> move(R) |> move_n(n - 1)
  | _zero => z
  };
};

let select = (d: Dir.t, z: EZipper.t): option(EZipper.t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.foc || Ziggurat.is_empty(z.sel)) {
    let+ (c, rel) = Stepwell.uncons_char(~from=d, z.rel);
    // let bs = Stepwell.bounds(rel);
    let sel = push_sel(c, d, z.sel);
    mk(~foc=d, ~sel, rel);
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(pull_sel(~char=true, z.foc, z.sel));
    let ctx =
      z.ctx |> Stepwell.cons_lexeme(~onto=b, c) |> Stepwell.assemble(~sel);
    return(mk(~sel, ctx));
  };
};

let insert_piece = (ctx: EStepwell.t, ~slot=ESlot.Empty, p: Piece.Labeled.t) => {
  let (ctx, lt) =
    switch (Molder.Stepwell.mold(ctx, ~slot, p)) {
    | Ok(ok) => ok
    | Error(_) when Piece.is_empty(p) => (ctx, [])
    | Error(_) => failwith("bug: failed to mold")
    };
  EStepwell.cat_slopes((lt, []), ctx);
};
let reinsert_piece = (ctx, ~slot=ESlot.Empty, p: Piece.t) =>
  insert_piece(ctx, ~slot, Piece.to_labeled(p));

let rec remold_ctx = (~slot=ESlot.Empty, ctx: EStepwell.t): EStepwell.t =>
  switch (EStepwell.pop_terrace(~from=R, ctx)) {
  | None => EStepwell.cat_slopes((ESlope.Dn.unroll(slot), []), ctx)
  | Some((t, ctx)) =>
    let (face, rest) = EWald.split_face(~side=L, t.wald);
    let inserted = reinsert_piece(ctx, ~slot, face);
    switch (EStepwell.face(~side=L, inserted)) {
    | Some(p) when p.material == face.material =>
      // fast path for when face piece retains mold
      inserted
      |> EStepwell.extend_face(~side=L, rest)
      |> remold_ctx(~slot=t.slot)
    | _ =>
      // otherwise add rest of wald to suffix queue
      let up =
        switch (rest) {
        | ([], _) => []
        | ([slot, ...slots], ps) =>
          let t = {...t, wald: Wald.mk(ps, slots)};
          ESlope.Up.(cat(unroll(slot), [t]));
        };
      ctx |> EStepwell.cat_slopes(([], up)) |> remold_ctx;
    };
  };

let save_cursor = (ctx: EStepwell.t) => {
  let w = Wald.singleton(Piece.mk_cursor());
  Melder.Stepwell.meld_or_fail(~onto=L, w, ctx);
};

let load_cursor = ctx => ctx |> EZipper.zip |> EZipper.unzip;

// d is side of cleared focus contents the cursor should end up
let clear_focus = (d: Dir.t, z: EZipper.t): EStepwell.t => {
  // let ctx = Melder.Stepwell.rebridge(z.ctx);
  let b = Dir.toggle(d);
  let ws = EZipper.Focus.clear(z.foc);
  let meld = Melder.Stepwell.meld_or_fail(~onto=b);
  switch (b) {
  | L => List.fold_left(Fun.flip(meld), ctx, ws)
  | R => List.fold_right(meld, ws, ctx)
  };
};

let propagate_change = (ctx: EStepwell.t) =>
  ctx |> save_cursor |> remold_ctx |> load_cursor |> EZipper.mk;

let insert = (s: string, z: EZipper.t) => {
  let ctx = clear_focus(L, z);
  let (l, ctx) = EStepwell.pull_token(~from=L, ctx);
  let (r, ctx) = EStepwell.pull_token(~from=R, ctx);
  Lexer.lex(l ++ s ++ r)
  |> List.fold_left(insert_piece, ctx)
  |> propagate_change
  |> move_n(- Token.length(r));
};

let delete = (d: Dir.t, z: EZipper.t): option(EZipper.t) => {
  open OptUtil.Syntax;
  let+ z = EZipper.Focus.is_empty(z.foc) ? select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: EZipper.t): option(EZipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
