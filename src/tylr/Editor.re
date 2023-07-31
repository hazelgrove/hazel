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

let rec relex_insert = (s: string, rel: Stepwell.t): (Lexed.t, Stepwell.t) => {
  print_endline("Stepwell.relex_insert");
  assert(s != "");
  let ((l, r), rel') = Stepwell.uncons_opt_lexemes(rel);
  switch (l, r) {
  | (Some(G(l)), Some(G(r))) when l.id == r.id =>
    print_endline("Stepwell.relex_insert / grout mid");
    let prefix = l.fill ++ s;
    switch (fill(prefix, r)) {
    | None =>
      relex_insert(
        prefix,
        cons(~onto=R, Terrace.of_piece(Piece.of_grout(r)), rel'),
      )
    | Some(p) => (
        Lexed.empty,
        Stepwell.cons(~onto=L, Terrace.of_piece(p), rel'),
      )
    };
  | (_, Some(G(r))) when Option.is_some(fill(s, r)) =>
    print_endline("Stepwell.relex_insert / grout start");
    let p = Option.get(fill(s, r));
    let rel =
      rel'
      |> Stepwell.cons_opt_lexeme(~onto=L, l)
      |> Stepwell.cons(~onto=L, Terrace.of_piece(p))
      |> (
        Piece.is_grout(p)
          ? Stepwell.cons(~onto=R, Terrace.of_piece(Piece.of_grout(r)))
          : Fun.id
      );
    (Lexed.empty, rel);
  | _ =>
    print_endline("Stepwell.relex_insert / not filling");
    // todo: recycle ids + avoid remolding if unaffected
    let (tok_l, rel) =
      switch (l) {
      | None => ("", rel')
      | Some(T(t)) => (t.token, rel')
      | Some(l) => ("", Stepwell.cons_lexeme(~onto=L, l, rel'))
      };
    let (tok_r, rel) =
      switch (r) {
      | None => ("", rel)
      | Some(T(t)) => (t.token, rel)
      | Some(r) => ("", Stepwell.cons_lexeme(~onto=R, r, rel))
      };
    let lexed = (Lexer.lex(tok_l ++ s ++ tok_r), Token.length(tok_r));
    print_endline("lexed = " ++ Lexeme.show_s(fst(lexed)));
    (lexed, rel);
  };
};
let relex = (~insert="", rel: Stepwell.t): (Lexed.t, Stepwell.t) => {
  print_endline("Stepwell.relex");
  switch (insert) {
  | "" =>
    let ((l, r), rel') = Stepwell.uncons_opt_lexemes(rel);
    switch (l, r) {
    | (None | Some(S(_) | G(_)), _)
    | (_, None | Some(S(_) | G(_))) => (Lexed.empty, rel)
    | (Some(T(l)), Some(T(r))) =>
      switch (Lexer.lex(l.token ++ r.token)) {
      | [T(l'), T(r')] when l'.token == l.token && r'.token == r.token => (
          Lexed.empty,
          rel,
        )
      | ls => ((ls, Token.length(r.token)), rel')
      }
    };
  | _ => relex_insert(insert, rel)
  };
};

let delete = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let+ z = Ziggurat.is_empty(z.sel) ? select(d, z) : return(z);
  let (lexed, ctx) = z.ctx |> Stepwell.assemble |> Stepwell.relex;
  // selection dropped
  mk(Stepwell.insert(lexed, ctx));
};

let insert = (s: string, z: Zipper.t): Zipper.t => {
  print_endline("Zipper.insert");
  // delete (ignore) sel and reassemble ctx if needed
  let ctx = Ziggurat.is_empty(z.sel) ? z.ctx : Stepwell.assemble(z.ctx);
  let (lexed, ctx) = Stepwell.relex(~insert=s, ctx);
  let rel =
    switch (lexed |> List.map(Lexeme.is_space) |> OptUtil.sequence) {
    | Some(s) =>
      ctx
      |> push_space(~onto=L, Space.concat(s))
      // remold if deletion, otherwise
      // fast path for space-only insertion
      |> (lexed == [] ? remold_suffix : Fun.id)
    | None =>
      print_endline("Stepwell.insert / not space");
      let inserted = List.fold_left(Fun.flip(insert_lexeme), ctx, lexed);
      print_endline("inserted = " ++ show(inserted));
      // let ins_path = path(inserted);
      // print_endline("ins_path = " ++ Meld.Path.show(ins_path));
      let marked = mark(inserted);
      print_endline("marked = " ++ show(marked));
      let remolded = remold_suffix(marked);
      print_endline("remolded = " ++ show(remolded));
      let zipped = zip(remolded);
      print_endline("zipped = " ++ Meld.show(zipped));
      Zipper.unzip(zipped);
    };
  let rel = FunUtil.(repeat(offset, force_opt(shift_char(~from=L)), rel));
  // selection dropped
  mk(rel);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
