module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: Zipper.t): option(Zipper.t) =>
  if (!Ziggurat.is_empty(z.sel)) {
    Some(Zipper.unselect(d, z));
  } else {
    open OptUtil.Syntax;
    let+ (c, ctx) = Stepwell.pull_lexeme(~char=true, ~from=d, z.ctx);
    ctx
    |> Stepwell.push_lexeme(~onto=Dir.toggle(b), c)
    |> Stepwell.assemble
    |> mk;
  };
let rec move_n = (n: int, z: Zipper.t): option(Zipper.t) =>
  switch (n) {
  | _ when n < 0 => Option.bind(move(L, z), move_n(n + 1))
  | _ when n > 0 => Option.bind(move(R, z), move_n(n - 1))
  | _zero => Some(z)
  };

let select = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
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

let insert = (s: string, z: Zipper.t): Zipper.t => {
  let tok =
    fun
    | None => Token.empty
    | Some(p) => p.token;

  // delete (by ignoring) sel and rebridge if needed
  let ctx = Ziggurat.is_empty(z.sel) ? z.ctx : Stepwell.rebridge(z.ctx);

  // lex input + lexable neighbors
  let (l, ctx) = Stepwell.pull_lexable(~from=L, ctx);
  let (r, ctx) = Stepwell.pull_lexable(~from=R, ctx);
  let ls = Lexer.lex(tok(l) ++ s ++ tok(r));

  // fast path + id continuity for extending token to left
  let (ls, ctx) =
    switch (l, ls) {
    | (Some(p), [T((lbl, token)), ...tl]) when Piece.label(p) == Some(lbl) => (
        tl,
        Stepwell.push_piece(~onto=L, {...p, token}, ctx),
      )
    | _ => (ls, ctx)
    };

  // insert remaining lexemes
  ls
  |> List.fold_left((ctx, lx) => insert_lexeme(lx, ctx), ctx)
  |> Zipper.mk
  // restore cursor position
  |> move_n(- Token.length(tok(r)))
  |> OptUtil.get_or_fail("bug: lost cursor position");
};

let delete = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let+ z = Ziggurat.is_empty(z.sel) ? select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
