open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Slopes.t, Bridge.t);

// base slopes
let of_slopes: Slopes.t => t = Chain.of_loop;
let get_slopes: t => Slopes.t = Chain.fst;
let map_slopes: (_, t) => t = Chain.map_fst;
let put_slopes = sib => map_slopes(_ => sib);
let cons_slopes = (sib, rel) => map_slopes(Slopes.cat(sib), rel);

let cons_bridge = bridge => Chain.link(Slopes.empty, bridge);

let empty = of_slopes(Slopes.empty);

let cat: (t, t) => t = Chain.cat(Slopes.cat);
let concat = (rels: list(t)) => List.fold_right(cat, rels, empty);

let rec cons_l = (~kid=Meld.empty(), mel: Terrace.L.t, rel: t): t => {
  open Slope;
  let (dn, up) = get_slopes(rel);
  switch (Dn.snoc(dn, ~kid, mel)) {
  | Ok(dn) => put_slopes((dn, up), rel)
  | Error(kid) =>
    switch (Chain.unlink(rel)) {
    | None =>
      let dn = Dn.of_meld(Terrace.L.unmk(kid, mel));
      put_slopes((dn, up), rel);
    | Some((_slopes, (l, r), rel)) =>
      switch (Terrace.cmp(l, ~kid, mel)) {
      | None => raise(Bridge.Convex_inner_tips)
      | Some(Lt(kid_mel)) => put_slopes((Dn.of_meld(kid_mel), up), rel)
      | Some(Eq(l_kid_mel)) =>
        let dn = Dn.of_meld(l_kid_mel);
        let up = Up.cat(up, Up.of_terr(r));
        cons_slopes((dn, up), rel);
      | Some(Gt(l_kid)) =>
        let up = Up.cat(up, Up.of_terr(r));
        rel |> cons_slopes(Slopes.mk(~r=up, ())) |> cons_l(~kid=l_kid, mel);
      }
    }
  };
};
let rec cons_r = (~kid=Meld.empty(), mel: Terrace.R.t, rel: t): t => {
  open Slope; // left-to-right: mel kid
  let (dn, up) = get_slopes(rel);
  switch (Up.cons(mel, ~kid, up)) {
  | Ok(up) => put_slopes((dn, up), rel)
  | Error(kid) =>
    switch (Chain.unlink(rel)) {
    | None =>
      let up = Up.of_meld(Terrace.R.unmk(mel, kid));
      put_slopes((dn, up), rel);
    | Some((_slopes, (l, r), rel)) =>
      switch (Terrace.cmp(mel, ~kid, r)) {
      | None => raise(Bridge.Convex_inner_tips)
      | Some(Gt(mel_kid)) => put_slopes((dn, Up.of_meld(mel_kid)), rel)
      | Some(Eq(mel_kid_r)) =>
        let dn = Dn.cat(Dn.of_terr(l), dn);
        let up = Up.of_meld(mel_kid_r);
        cons_slopes((dn, up), rel);
      | Some(Lt(kid_r)) =>
        let dn = Dn.cat(Dn.of_terr(l), dn);
        rel |> cons_slopes(Slopes.mk(~l=dn, ())) |> cons_r(mel, ~kid=kid_r);
      }
    }
  };
};
let cons = (~onto: Dir.t, terr, rel) =>
  switch (onto) {
  | L => rel |> cons_l(terr)
  | R => rel |> cons_r(terr)
  };
let cons_space = (~onto: Dir.t, s, rel) =>
  rel |> map_slopes(Slopes.cons_space(~onto, s));

// todo: rename relative to cons_slopes
let cons_slope = (~onto: Dir.t, slope: Slope.t, rel) =>
  List.fold_left(
    (rel, terr) => cons(~onto, terr, rel),
    rel |> cons_space(~onto, slope.space),
    slope.terrs,
  );

let cons_zigg = (~onto: Dir.t, {up, top, dn}: Ziggurat.t, rel) => {
  let cons_top =
    switch (top) {
    | None => Fun.id
    | Some(top) => cons(~onto, Terrace.of_wald(top))
    };
  switch (onto) {
  | L =>
    rel
    |> cons_slope(~onto, up)
    |> cons_top
    |> cons_slopes(Slopes.mk(~l=dn, ()))
  | R =>
    rel
    |> cons_slope(~onto, dn)
    |> cons_top
    |> cons_slopes(Slopes.mk(~r=up, ()))
  };
};

let rec unzip_slopes = ((l, r) as slopes, well) =>
  switch (Slope.Dn.uncons(l), Slope.Up.unsnoc(r)) {
  | (None, _)
  | (_, None) => cons_slopes(slopes, well)
  | (Some((hd_l, tl_l)), Some((tl_r, hd_r))) =>
    switch (Terrace.cmp(hd_l, hd_r)) {
    | None => failwith("expected cmp")
    | Some(Eq(_)) =>
      well |> cons_bridge((hd_l, hd_r)) |> unzip_slopes((tl_l, tl_r))
    | Some(Lt(_)) =>
      well
      |> cons_slopes(Slopes.mk(~l=Slope.of_terr(hd_l), ()))
      |> unzip_slopes((tl_l, r))
    | Some(Gt(_)) =>
      well
      |> cons_slopes(Slopes.mk(~r=Slope.of_terr(hd_r), ()))
      |> unzip_slopes((l, tl_r))
    }
  };

let assemble = (~sel=Ziggurat.empty, rel: t): t => {
  print_endline("Stepwell.assemble");
  let (pre, suf) = get_slopes(rel);
  // separate siblings that belong to the selection
  let (pre_lt_sel, pre_geq_sel) = Ziggurat.split_lt(pre, sel);
  let (sel_leq_suf, sel_gt_suf) = Ziggurat.split_gt(sel, suf);
  rel
  |> put_slopes(Slopes.empty)
  |> unzip_slopes((pre_lt_sel, sel_gt_suf))
  |> cons_slopes((pre_geq_sel, sel_leq_suf));
};

let cons_lexeme = (~onto: Dir.t, lx: Lexeme.t(_)) =>
  switch (lx) {
  | S(s) => cons_space(~onto, s)
  | T(p) => cons(~onto, Terrace.of_piece(p))
  };
let cons_opt_lexeme = (~onto: Dir.t, lx) =>
  switch (lx) {
  | None => Fun.id
  | Some(lx) => cons_lexeme(~onto, lx)
  };

let uncons_lexeme = (~char=false, ~from: Dir.t, rel) =>
  switch (Slopes.uncons_lexeme(~char, ~from, get_slopes(rel))) {
  | Some((a, sib)) => Some((a, put_slopes(sib, rel)))
  | None =>
    open OptUtil.Syntax;
    let+ (sib, par, rel) = Chain.unlink(rel);
    let (a, par) = Bridge.uncons_lexeme(~char, ~from, par);
    let rel = rel |> cons_slopes(Slopes.cat(sib, par)) |> assemble;
    (a, rel);
  };
let uncons_opt_lexeme = (~char=false, ~from, rel) =>
  switch (uncons_lexeme(~from, rel)) {
  | None => (None, rel)
  | Some((l, rel)) => (Some(l), rel)
  };
let uncons_opt_lexemes =
    (~char=false, rel: t): ((option(Lexeme.t(Piece.t)) as 'l, 'l), t) => {
  let (l, rel) = uncons_opt_lexeme(~from=L, rel);
  let (r, rel) = uncons_opt_lexeme(~from=R, rel);
  ((l, r), rel);
};

let shift_char = (~from: Dir.t, rel: t) => {
  open OptUtil.Syntax;
  let+ (c, rel) = uncons_lexeme(~char=true, ~from, rel);
  cons_lexeme(~onto=Dir.toggle(from), c, rel);
};

let rec shift_chars = (n, rel) =>
  if (n < 0) {
    shift_char(~from=L, rel) |> OptUtil.and_then(shift_chars(n + 1));
  } else if (n > 0) {
    shift_char(~from=R, rel) |> OptUtil.and_then(shift_chars(n - 1));
  } else {
    Some(rel);
  };

// if until is None, attempt to shift a single spiece.
// if until is Some(f), shift spieces until f succeeds or until no spieces left to shift.
// note the latter case always returns Some.
// let rec shift_lexeme =
//         (~until: option((Lexeme.t, t) => option(t))=?, ~from: Dir.t, rel: t)
//         : option(t) => {
//   let onto = Dir.toggle(from);
//   switch (until, uncons_lexeme(~from, rel)) {
//   | (None, None) => None
//   | (Some(_), None) => Some(rel)
//   | (None, Some((l, rel))) => Some(cons_lexeme(~onto, l, rel))
//   | (Some(f), Some((l, rel))) =>
//     switch (f(l, rel)) {
//     | Some(rel) => Some(rel)
//     | None => rel |> cons_lexeme(~onto, l) |> shift_lexeme(~from, ~until?)
//     }
//   };
// };

let mold_lt = (~kid=?, t: Token.t, rel: t): Result.t(Mold.t, option(Sort.o)) => {
  open Result.Syntax;
  let (pre, _) = get_slopes(rel);
  let/ kid = Slope.Dn.mold_lt(pre, ~kid?, t);
  switch (Chain.unlink(rel)) {
  | None => Error(kid)
  | Some((_slopes, bridge, _)) => Bridge.mold_lt(~kid?, t, bridge)
  };
};
let rec mold_eq =
        (~kid=?, t: Token.t, rel: t): Result.t(Mold.t, option(Sort.o)) => {
  open Result.Syntax;
  let (pre, _) = get_slopes(rel);
  let/ kid = Slope.Dn.mold(pre, ~kid?, t);
  switch (Chain.unlink(rel)) {
  | None => Error(kid)
  | Some((_slopes, bridge, rel)) =>
    let/ kid = Bridge.mold_eq(~kid?, t, bridge);
    mold_eq(~kid?, t, rel);
  };
};
let mold__ = (t: Token.t, rel: t): Mold.t =>
  switch (mold_eq(t, rel)) {
  | Ok(m) => m
  | Error(_) =>
    switch (mold_lt(t, rel)) {
    | Ok(m) => m
    | Error(_) =>
      Token.is_default_convex(t) ? Mold.default_operand : Mold.default_infix
    }
  };

let bounds = (rel: t): (option(Terrace.R.t), option(Terrace.L.t)) => {
  let bounds = Slopes.bounds(get_slopes(rel));
  switch (bounds, Chain.unlink(rel)) {
  | (_, None)
  | ((Some(_), Some(_)), _) => bounds
  | ((None, _) | (_, None), Some((_, (l, r), _))) =>
    let l = Option.value(fst(bounds), ~default=l);
    let r = Option.value(snd(bounds), ~default=r);
    (Some(l), Some(r));
  };
};

let rec insert_terr = (~complement, terr: Terrace.L.t, rel: t): t => {
  let (p, rest) = Terrace.L.split_face(terr);
  switch (p.shape) {
  | G(_) =>
    // todo: may need to remold?
    rel
    |> cons(~onto=L, Terrace.of_piece(p))
    |> insert_up(~complement, Slope.Up.of_meld(rest))
  | T(t) =>
    switch (mold(t.proto.label, rel)) {
    | Ok(m) when m == t.proto.mold =>
      // todo: need to strengthen this fast check to include completeness check on kids
      cons(~onto=L, terr, rel)
    | m =>
      let t =
        switch (m) {
        | Error(_) => t
        | Ok(mold) => {
            ...t,
            proto: {
              ...t.proto,
              mold,
            },
          }
        };
      rel
      |> cons(~onto=L, Terrace.of_piece(Piece.of_tile(~paths=p.paths, t)))
      |> insert_up(~complement, Slope.Up.of_meld(rest));
    }
  };
}
and insert_complement = (rel: t) => {
  let (pre, _) = get_slopes(rel);
  Slope.Dn.complement(pre)
  |> List.map(proto =>
       Tile.mk(~unfilled=Proto.length(proto), proto)
       |> Piece.of_tile
       |> Terrace.of_piece
     )
  |> List.fold_left(Fun.flip(insert_terr(~complement=false)), rel);
}
and insert_space = (~complement, s: Space.t, rel: t) =>
  s
  |> Space.fold_left(
       rel,
       (rel, s: Space.t) =>
         rel
         |> (
           switch (s.chars) {
           | [{shape: Newline, _}, ..._] when complement => insert_complement
           | _ => Fun.id
           }
         )
         |> cons_space(~onto=L, s),
       Fun.flip(cons_space(~onto=L)),
     )
and insert_up = (~complement, up: Slope.Up.t, rel: t): t =>
  up
  |> Slope.Up.fold(
       s => insert_space(~complement, s, rel),
       (rel, t) => insert_terr(~complement, t, rel),
     );

let insert_lexeme = (~complement=false, lx: Lexeme.t(Token.t), rel: t): t =>
  switch (lx) {
  | S(s) => insert_space(~complement, s, rel)
  | T(t) => insert_terr(~complement, Terrace.of_piece(p), rel)
  };

let regrout = rel => {
  print_endline("Stepwell.regrout");
  let sib_of_g = g =>
    Slopes.mk(~r=Slope.Up.of_piece(Piece.of_grout(g)), ());
  let sib_of_l = t =>
    switch (Terrace.R.tip(t)) {
    | Convex => Slopes.empty
    | Concave(sort, _) => sib_of_g(Grout.mk_operand(sort))
    };
  let sib_of_r = t =>
    switch (Terrace.L.tip(t)) {
    | Convex => Slopes.empty
    | Concave(sort, _) => sib_of_g(Grout.mk_operand(sort))
    };
  let sib =
    switch (bounds(rel)) {
    | (None, None) => sib_of_g(Grout.mk_operand(Sort.root_o))
    | (None, Some(r)) => sib_of_r(r)
    | (Some(l), None) => sib_of_l(l)
    | (Some(l), Some(r)) =>
      switch (Terrace.cmp(l, r)) {
      | None =>
        let sort = Sort.lca(Terrace.sort(l), Terrace.sort(r));
        let prec = min(Terrace.prec(l), Terrace.prec(r));
        sib_of_g(Grout.mk_infix(sort, prec));
      | Some(Lt(_)) => sib_of_r(r)
      | Some(Gt(_)) => sib_of_l(l)
      | Some(Eq(_)) =>
        Piece.(id(Terrace.R.face(l)) == id(Terrace.L.face(r)))
          ? Slopes.empty
          // todo: review sort
          : sib_of_g(Grout.mk_operand(None))
      }
    };
  cons_slopes(sib, rel);
};

let rec remold_suffix = (rel: t): t => {
  let (pre, suf) = get_slopes(rel);
  switch (suf.terrs) {
  | [] => rel |> insert_complement |> regrout
  | [terr, ...terrs] =>
    rel
    |> put_slopes((pre, Slope.Up.mk(terrs)))
    |> insert_space(~complement=true, suf.space)
    // note: insertion may flatten ancestors into siblings, in which
    // case additional elements may be added to suffix to be remolded
    // (safe bc rel is decreasing in height).
    |> insert_terr(~complement=true, terr)
    |> remold_suffix
  };
};

let fill = (s: string, g: Grout.t): option(Piece.t) =>
  if (String.equal(s, g.sugg)) {
    Some(Piece.mk(T(Tile.mk(~id=g.id, g.mold, s))));
  } else if (String.starts_with(~prefix=s, g.sugg)) {
    Some(Piece.mk(G(Grout.mk(~id=g.id, ~fill=s, g.mold))));
  } else {
    None;
  };

let zip = (rel: t): Meld.t =>
  rel
  |> Chain.fold_left(Slopes.zip_init, (zipped, par, sib) =>
       zipped |> Bridge.zip(par) |> Slopes.zip(sib)
     );

module Lexed = {
  open Sexplib.Std;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Lexeme.s, int);
  let empty = ([], 0);
};

let rec relex_insert = (s: string, rel: t): (Lexed.t, t) => {
  print_endline("Stepwell.relex_insert");
  assert(s != "");
  let ((l, r), rel') = uncons_opt_lexemes(rel);
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
    | Some(p) => (Lexed.empty, cons(~onto=L, Terrace.of_piece(p), rel'))
    };
  | (_, Some(G(r))) when Option.is_some(fill(s, r)) =>
    print_endline("Stepwell.relex_insert / grout start");
    let p = Option.get(fill(s, r));
    let rel =
      rel'
      |> cons_opt_lexeme(~onto=L, l)
      |> cons(~onto=L, Terrace.of_piece(p))
      |> (
        Piece.is_grout(p)
          ? cons(~onto=R, Terrace.of_piece(Piece.of_grout(r))) : Fun.id
      );
    (Lexed.empty, rel);
  | _ =>
    print_endline("Stepwell.relex_insert / not filling");
    // todo: recycle ids + avoid remolding if unaffected
    let (tok_l, rel) =
      switch (l) {
      | None => ("", rel')
      | Some(T(t)) => (t.token, rel')
      | Some(l) => ("", cons_lexeme(~onto=L, l, rel'))
      };
    let (tok_r, rel) =
      switch (r) {
      | None => ("", rel)
      | Some(T(t)) => (t.token, rel)
      | Some(r) => ("", cons_lexeme(~onto=R, r, rel))
      };
    let lexed = (Lexer.lex(tok_l ++ s ++ tok_r), Token.length(tok_r));
    print_endline("lexed = " ++ Lexeme.show_s(fst(lexed)));
    (lexed, rel);
  };
};
let relex = (~insert="", rel: t): (Lexed.t, t) => {
  print_endline("Stepwell.relex");
  switch (insert) {
  | "" =>
    let ((l, r), rel') = uncons_opt_lexemes(rel);
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

let mark = (rel: t): t => {
  switch (uncons_lexeme(~from=R, rel)) {
  | None => cons_space(~onto=R, Space.mk(~paths=[0], []), rel)
  | Some((lx, rel)) =>
    switch (Lexeme.to_piece(lx)) {
    | Error(s) =>
      let marked = Space.add_paths([0], s);
      cons_space(~onto=R, marked, rel);
    | Ok(p) =>
      let marked = Piece.add_paths([0], p);
      cons_r(Terrace.of_piece(marked), rel);
    }
  };
};

let unzip_end = (~unzipped, side: Dir.t, mel: Meld.t) =>
  unzipped
  |> cons_slopes(
       side == L
         ? Slope.(Dn.empty, Up.of_meld(mel))
         : Slope.(Dn.of_meld(mel), Up.empty),
     );
// |> mk;

// todo: standardize a la unzip_piece
let unzip_space = (s: Space.t, unzipped) => {
  let (paths, s) = (s.paths, Space.clear_paths(s));
  let (l, _sel, r) =
    switch (paths) {
    | [] => Space.(empty, empty, s)
    | [n] =>
      let (l, r) = Space.split(n, s);
      (l, Space.empty, r);
    | [m, n, ..._] =>
      assert(m <= n);
      let (s, r) = Space.split(n, s);
      let (l, s) = Space.split(m, s);
      (l, s, r);
    };
  unzipped |> cons_slopes(Slope.(Dn.mk(~s=l, []), Up.mk(~s=r, [])));
  // |> mk(~sel=Segment.s(sel));
};

let unzip_piece = (p: Piece.t) => {
  let (paths, p) = (p.paths, Piece.clear_paths(p));
  let ret = (~l=?, ~sel=?, ~r=?, ()) => (l, sel, r);
  let single_path = n =>
    switch (Piece.unzip(n, p)) {
    | L(L) => ret(~r=p, ())
    | L(R) => ret(~l=p, ())
    | R((l, r)) => ret(~l, ~r, ())
    };
  switch (paths) {
  | [] => ret(~r=p, ())
  | [n] => single_path(n)
  // only handling up to two paths marking selection atm
  | [m, n, ..._] =>
    assert(m <= n);
    switch (Piece.unzip(n, p)) {
    | L(L) => ret(~r=p, ())
    | L(R) => single_path(m)
    | R((p, r)) =>
      switch (Piece.unzip(m, p)) {
      | L(L) => ret(~sel=p, ~r, ())
      | L(R) => ret(~l=p, ~r, ())
      | R((l, p)) => ret(~l, ~sel=p, ~r, ())
      }
    };
  };
};

let unzip_lex = (lex: Path.Lex.t, mel: Meld.t, unzipped) =>
  switch (lex) {
  | Space(L) =>
    let (l, r) = mel.space;
    let mel = {...mel, space: (Space.empty, r)};
    unzipped
    |> cons_slopes(Slopes.mk(~r=Slope.Up.of_meld(mel), ()))
    |> unzip_space(l);
  | Space(R) =>
    let (l, r) = mel.space;
    let mel = {...mel, space: (l, Space.empty)};
    unzipped
    |> cons_slopes(Slopes.mk(~l=Slope.Dn.of_meld(mel), ()))
    |> unzip_space(r);
  | Piece(n) =>
    let (mel_l, p, mel_r) = Meld.split_piece(n, mel);
    // todo: restore for unzipping selections
    let (p_l, _p_sel, p_r) = unzip_piece(p);
    // let sel =
    //   p_sel
    //   |> Option.map(Segment.of_piece)
    //   |> Option.value(~default=Segment.empty);
    let l =
      p_l
      |> Option.map(p => Meld.knil(mel_l, p))
      |> Option.value(~default=mel_l)
      |> Slope.Dn.of_meld;
    let r =
      p_r
      |> Option.map(p => Meld.link(p, mel_r))
      |> Option.value(~default=mel_r)
      |> Slope.Up.of_meld;
    unzipped |> cons_slopes(Slopes.mk(~l, ~r, ()));
  // |> mk(~sel);
  };

let rec unzip = (~unzipped=empty, mel: Meld.t): t => {
  let (paths, mel) = (mel.paths, Meld.distribute_paths(mel));
  switch (Paths.hd_kid(paths)) {
  | Some(kid) =>
    let mel = Meld.distribute_space(mel);
    switch (Bridge.unzip(kid, mel)) {
    | Some((kid, b)) =>
      let unzipped = cons_bridge(b, unzipped);
      unzip(~unzipped, kid);
    | None =>
      let (kid, slopes) =
        if (kid == 0) {
          let (kid, t) =
            Terrace.L.mk(mel) |> OptUtil.get_or_raise(Path.Invalid);
          (kid, Slope.(Dn.empty, Up.of_terr(t)));
        } else {
          let (t, kid) =
            Terrace.R.mk(mel) |> OptUtil.get_or_raise(Path.Invalid);
          (kid, Slope.(Dn.of_terr(t), Up.empty));
        };
      let unzipped = cons_slopes(slopes, unzipped);
      unzip(~unzipped, kid);
    };
  | None =>
    switch (Paths.hd_lex(paths)) {
    | Some(lex) => unzip_lex(lex, mel, unzipped)
    | None =>
      // failwith("todo: unzipping selections")
      print_endline("zero or multiple paths found");
      unzip_end(~unzipped, L, mel);
    }
  };
};

let insert = ((ls, offset): Lexed.t, rel: t): t => {
  let rel =
    switch (ls |> List.map(Lexeme.is_space) |> OptUtil.sequence) {
    | Some(s) =>
      rel
      |> cons_space(~onto=L, Space.concat(s))
      // remold if deletion, otherwise
      // fast path for space-only insertion
      |> (ls == [] ? remold_suffix : Fun.id)
    | None =>
      print_endline("Stepwell.insert / not space");
      let inserted = List.fold_left(Fun.flip(insert_lexeme), rel, ls);
      print_endline("inserted = " ++ show(inserted));
      // let ins_path = path(inserted);
      // print_endline("ins_path = " ++ Meld.Path.show(ins_path));
      let marked = mark(inserted);
      print_endline("marked = " ++ show(marked));
      let remolded = remold_suffix(marked);
      print_endline("remolded = " ++ show(remolded));
      let zipped = zip(remolded);
      print_endline("zipped = " ++ Meld.show(zipped));
      unzip(zipped);
    };
  FunUtil.(repeat(offset, force_opt(shift_char(~from=L)), rel));
};
