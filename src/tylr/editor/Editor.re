module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: EZipper.t): option(EZipper.t) =>
  if (!Ziggurat.is_empty(z.sel)) {
    Some(EZipper.unselect(d, z));
  } else {
    open OptUtil.Syntax;
    let+ (c, ctx) = Stepwell.pull_lexeme(~char=true, ~from=d, z.ctx);
    ctx
    |> Stepwell.push_lexeme(~onto=Dir.toggle(b), c)
    |> Stepwell.assemble
    |> mk;
  };
let rec move_n = (n: int, z: EZipper.t): option(EZipper.t) =>
  switch (n) {
  | _ when n < 0 => Option.bind(move(L, z), move_n(n + 1))
  | _ when n > 0 => Option.bind(move(R, z), move_n(n - 1))
  | _zero => Some(z)
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

let insert_piece = (well: EStepwell.t, ~slot=ESlot.Empty, p: Piece.t) =>
  switch (Molder.Stepwell.mold(well, ~slot, p)) {
  | Ok((well, lt)) => EStepwell.cat_slopes((lt, []), well)
  | Error(_) when Piece.is_unfinished(p) =>
    // redundant obligation, remove
    EStepwell.cat_slopes((ESlope.Dn.unroll(slot), []), well);
  | Error(_) =>
    let w = Wald.singleton(Piece.unmold(p));
    well
    |> EStepwell.cat_slopes((ESlope.Dn.unroll(slot), []))
    |> Melder.Stepwell.meld_or_fail(~onto=L, w);
  };

let rec remold_ctx = (~slot=ESlot.Empty, well: EStepwell.t): EStepwell.t =>
  switch (EStepwell.pop_terrace(~from=R, well)) {
  | None => EStepwell.cat_slopes((ESlope.Dn.unroll(slot), []), well)
  | Some((t, well)) =>
    let (face, rest) = EWald.split_face(~side=L, hd.wald);
    let inserted = insert_piece(well, ~slot, face);
    switch (EStepwell.face(~side=L, inserted)) {
    | Some(p) when p.material == face.material =>
      // fast path for when face piece retains mold
      inserted |> EStepwell.extend_face(~side=L, rest) |> remold_ctx(~slot=t.slot)
    | _ =>
      // otherwise add rest of wald to suffix queue
      let up =
        switch (rest) {
        | ([], _) => []
        | ([slot, ...slots], ps) =>
          let t = {...t, wald: Wald.mk(ps, slots)};
          ESlope.Up.(cat(unroll(slot), [t]));
        };
      well |> EStepwell.cat_slopes(([], up)) |> remold_ctx;
    };
  };

let save_cursor = (ctx: EStepwell.t) => {
  let w = Wald.singleton(Piece.mk_cursor());
  Melder.Stepwell.meld_or_fail(~onto=L, w, ctx);
};

let load_cursor = (~offset=0, ctx) =>
  ctx
  |> EZipper.zip
  |> EZipper.unzip
  |> move_n(offset)
  |> OptUtil.get_or_fail("bug: lost cursor position");

let insert = (s: string, z: EZipper.t) => {
  // delete (by ignoring) sel and rebridge if needed
  // todo: replace selection contents with obligations
  let ctx = Ziggurat.is_empty(z.sel) ? z.ctx : Stepwell.rebridge(z.ctx);
  let (l, ctx) = EStepwell.pull_lexable(~from=L, ctx);
  let (r, ctx) = EStepwell.pull_lexable(~from=R, ctx);
  Lexer.lex(l ++ s ++ r)
  |> List.fold_left(insert_piece, ctx)
  |> save_cursor
  |> remold_ctx
  |> load_cursor(~offset=- Token.length(r))
  |> EZipper.mk;
};

let delete = (d: Dir.t, z: EZipper.t): option(EZipper.t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: EZipper.t): option(EZipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
