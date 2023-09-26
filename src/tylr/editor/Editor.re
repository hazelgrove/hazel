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

let insert_token = ((lbl: Label.t, t: Token.t), ctx: Stepwell.t): Stepwell.t =>
  switch (Stepwell.pull_lexeme(~from=R, ctx)) {
  | Some((T(p), ctx)) when Label.is_prefix(t, Piece.label(p)) =>
    let p = {...p, token: t};
    Melder.Stepwell.push(~onto=L, Wald.of_piece(p), ctx);
  | _ =>
    let m =
      switch (Molder.Stepwell.mold(well, t)) {
      | Some(m) => m
      | None => failwith("default grout mold based on token")
      };
    let p = Piece.mk(m, t);
    Melder.Stepwell.push(~onto=L, Wald.of_piece(p), well);
  };

let rec insert_wald = (wald: Wald.t, well: Stepwell.t): t => {
  let hd = Chain.fst(wald);
  switch (Molder.stepwell_mold(well, hd.token)) {
  | Ok(well) when Mold.eq(Stepwell.face(L, well).mold, hd.mold) =>
    Stepwell.put_wald(L, wald, well)
  | Error(_) =>
    switch (Wald.unlink(wald)) {
    | None => insert_token(hd.token, well)
    | Some((_hd, kid, tl)) => well |> insert_meld(kid) |> insert_wald(tl)
    }
  };
}
and insert_meld = (meld: Meld.t, well: Stepwell.t): t =>
  switch (Wald.mk(meld)) {
  | None => well // empty meld
  | Some((l, wald, r)) =>
    well |> insert_meld(l) |> insert_wald(wald) |> insert_meld(r)
  };

// todo: review complement flag
let insert_lexeme =
    (~complement=false, lx: Lexeme.t(Token.t), well: Stepwell.t): Stepwell.t =>
  switch (lx) {
  | S(s) => insert_space(~complement, s, well)
  | T(t) => insert_token(t, well)
  };

// let mark = (rel: t): t => {
//   switch (uncons_lexeme(~from=R, rel)) {
//   | None => cons_space(~onto=R, Space.mk(~paths=[0], []), rel)
//   | Some((lx, rel)) =>
//     switch (Lexeme.to_piece(lx)) {
//     | Error(s) =>
//       let marked = Space.add_paths([0], s);
//       cons_space(~onto=R, marked, rel);
//     | Ok(p) =>
//       let marked = Piece.add_paths([0], p);
//       cons_r(Terrace.of_piece(marked), rel);
//     }
//   };
// };
let mark = _ => failwith("todo: implement after grout molding");

let rec remold = (ctx: Stepwell.t): Stepwell.t =>
  switch (Stepwell.get_slopes(ctx)) {
  | (_, {terrs: [], _}) => insert_complement(ctx)
  | (pre, {terrs: [hd, ...tl], space}) =>
    ctx
    |> put_slopes((pre, Slope.Up.mk(tl)))
    |> insert_space(~complement=true, space)
    // note: insertion may break bridges, in which case
    // additional elements may be added to suffix to be remolded.
    // this recursion is safe bc ctx is always decreasing in
    // either suffix length or number of bridges.
    |> insert_wald(hd.wald)
    |> insert_meld(hd.meld)
    |> remold
  };

let insert_labeled = (ctx: EStepwell.t, l: Labeled.t) => {
  let (ctx, dn) = Molder.Stepwell.mold(ctx, l);
  EStepwell.map_slopes(Slopes.cat((dn, [])));
};

let remold = (ctx: EStepwell.t) => {
  EStepwell.mark(ctx)
  |> Molder.Stepwell.remold
  |> EZipper.zip
  |> EZipper.unzip;
};

let insert_piece = (well: EStepwell.t, ~slot=ESlot.Empty, p: Piece.t) =>
  switch (Molder.Stepwell.mold(well, ~slot, p)) {
  | Ok((well, lt)) => EStepwell.cat_slopes((lt, []), well)
  | Error(_) =>
    let w = Wald.singleton(Piece.unmold(p));
    well
    |> EStepwell.cat_slopes((ESlope.Dn.unroll(slot), []))
    |> Melder.Stepwell.meld_or_fail(~onto=L, w);
  };

let rec remold = (~slot=ESlot.Empty, well: EStepwell.t): EStepwell.t =>
  switch (EStepwell.pop_terrace(~from=R, well)) {
  | None => EStepwell.cat_slopes((ESlope.Dn.unroll(slot), []), well)
  | Some((t, well)) =>
    let (face, rest) = EWald.split_face(~side=L, hd.wald);
    let well = insert(well, ~slot, face);
    switch (EStepwell.face(~side=L, well)) {
    | Some(p) when p.material == face.material =>
      // fast path for when face piece retains mold
      well |> EStepwell.extend_face(~side=L, rest) |> remold(~slot=t.slot)
    | _ =>
      // otherwise add rest of wald to suffix queue
      let up =
        switch (rest) {
        | ([], _) => []
        | ([slot, ...slots], ps) =>
          let t = {...t, wald: Wald.mk(ps, slots)};
          ESlope.Up.(cat(unroll(slot), [t]));
        };
      well |> EStepwell.cat_slopes(([], up)) |> remold;
    };
  };

let restore_cursor = (~offset=0, ctx) =>
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
  |> remold
  |> restore_cursor(~offset=- Token.length(r));
};

let delete = (d: Dir.t, z: EZipper.t): option(EZipper.t) => {
  open OptUtil.Syntax;
  let+ z = Ziggurat.is_empty(z.sel) ? select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: EZipper.t): option(EZipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
