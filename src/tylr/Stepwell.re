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
      | {lt: None, eq: None, gt: None} => raise(Bridge.Convex_inner_tips)
      | {lt: Some(kid_mel), _} =>
        put_slopes((Dn.of_meld(kid_mel), up), rel)
      | {eq: Some(l_kid_mel), _} =>
        let dn = Dn.of_meld(l_kid_mel);
        let up = Up.cat(up, Up.of_terr(r));
        cons_slopes((dn, up), rel);
      | {gt: Some(l_kid), _} =>
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
      | {lt: None, eq: None, gt: None} => raise(Bridge.Convex_inner_tips)
      | {gt: Some(mel_kid), _} =>
        put_slopes((dn, Up.of_meld(mel_kid)), rel)
      | {eq: Some(mel_kid_r), _} =>
        let dn = Dn.cat(Dn.of_terr(l), dn);
        let up = Up.of_meld(mel_kid_r);
        cons_slopes((dn, up), rel);
      | {lt: Some(kid_r), _} =>
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

let rec unzip_slopes = ((l, r) as slopes, well) =>
  switch (Slope.Dn.uncons(l), Slope.Up.unsnoc(r)) {
  | (None, _)
  | (_, None) => cons_slopes(slopes, well)
  | (Some((hd_l, tl_l)), Some((tl_r, hd_r))) =>
    switch (Terrace.cmp(hd_l, hd_r)) {
    | {lt: None, eq: None, gt: None} => failwith("expected cmp")
    | {eq: Some(_), _} =>
      well |> cons_bridge((hd_l, hd_r)) |> unzip_slopes((tl_l, tl_r))
    | {lt: Some(_), _} =>
      well
      |> cons_slopes(Slopes.mk(~l=Slope.of_terr(hd_l), ()))
      |> unzip_slopes((tl_l, r))
    | {gt: Some(_), _} =>
      well
      |> cons_slopes(Slopes.mk(~r=Slope.of_terr(hd_r), ()))
      |> unzip_slopes((l, tl_r))
    }
  };

let assemble = (~sel=Segment.empty, rel: t): t => {
  let (pre, suf) = get_slopes(rel);
  // separate siblings that belong to the selection
  let (pre_lt_sel, pre_geq_sel) = Segment.split_lt(pre, sel);
  let (sel_leq_suf, sel_gt_suf) = Segment.split_gt(sel, suf);
  rel
  |> put_slopes(Slopes.empty)
  |> unzip_slopes((pre_lt_sel, sel_gt_suf))
  |> cons_slopes((pre_geq_sel, sel_leq_suf));
};

let cons_lexeme = (~onto: Dir.t, lx: Lexeme.t) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => cons_space(~onto, s)
  | Ok(p) => cons(~onto, Terrace.of_piece(p))
  };
let cons_opt_lexeme = (~onto: Dir.t, lx) =>
  switch (lx) {
  | None => Fun.id
  | Some(lx) => cons_lexeme(~onto, lx)
  };

let uncons = (~from_slopes, ~from_par, ~from: Dir.t, rel: t) =>
  switch (from_slopes(~from, get_slopes(rel))) {
  | Some((a, sib)) => Some((a, put_slopes(sib, rel)))
  | None =>
    open OptUtil.Syntax;
    let+ (sib, par, rel) = Chain.unlink(rel);
    let (a, par) = from_par(~from, par);
    let rel = rel |> cons_slopes(Slopes.cat(sib, par)) |> assemble;
    (a, rel);
  };
let uncons_char =
  uncons(~from_slopes=Slopes.uncons_char, ~from_par=Bridge.uncons_char);
let uncons_lexeme =
  uncons(~from_slopes=Slopes.uncons_lexeme, ~from_par=Bridge.uncons_lexeme);
let uncons_opt_lexeme = (~from, rel) =>
  switch (uncons_lexeme(~from, rel)) {
  | None => (None, rel)
  | Some((l, rel)) => (Some(l), rel)
  };
let uncons_opt_lexemes = (rel: t): ((option(Lexeme.t) as 'l, 'l), t) => {
  let (l, rel) = uncons_opt_lexeme(~from=L, rel);
  let (r, rel) = uncons_opt_lexeme(~from=R, rel);
  ((l, r), rel);
};

let shift_char = (~from: Dir.t, rel) => {
  let go = () => {
    open OptUtil.Syntax;
    let+ (c, rel) = uncons_char(~from, rel);
    cons_lexeme(~onto=Dir.toggle(from), c, rel);
  };
  switch (from, uncons_opt_lexemes(rel)) {
  | (L, ((Some(G(g)), r), rel)) =>
    switch (r) {
    | Some(G(g')) when g.id == g'.id => go()
    | _ =>
      let (l, r) = (g, {...g, fill: ""});
      rel
      |> cons_lexeme(~onto=L, G(l))
      |> cons_lexeme(~onto=R, G(r))
      |> Option.some;
    }
  | _ => go()
  };
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
let rec shift_lexeme =
        (~until: option((Lexeme.t, t) => option(t))=?, ~from: Dir.t, rel: t)
        : option(t) => {
  let onto = Dir.toggle(from);
  switch (until, uncons_lexeme(~from, rel)) {
  | (None, None) => None
  | (Some(_), None) => Some(rel)
  | (None, Some((l, rel))) => Some(cons_lexeme(~onto, l, rel))
  | (Some(f), Some((l, rel))) =>
    switch (f(l, rel)) {
    | Some(rel) => Some(rel)
    | None => rel |> cons_lexeme(~onto, l) |> shift_lexeme(~from, ~until?)
    }
  };
};

let mold_ =
    (~match, ~kid: option(Sort.o)=?, t: Token.t, rel: t)
    : Result.t(Mold.t, option(Sort.o)) => {
  let rec go = (~kid: option(Sort.o)=?, rel: t) => {
    open Result.Syntax;
    let (pre, _) = get_slopes(rel);
    let/ kid = Slope.Dn.mold(~match, pre, ~kid?, t);
    switch (Chain.unlink(rel)) {
    | None =>
      match
        ? Error(kid)
        : Result.of_option(
            ~error=kid,
            LangUtil.mold_of_token(kid, Sort.root_o, t),
          )
    | Some((_slopes, par, rel)) =>
      // todo: review whether match flag should be propagated to bridge
      let/ kid = Bridge.mold(~kid?, t, par);
      match ? go(~kid?, rel) : Error(kid);
    };
  };
  go(~kid?, rel);
};
let mold = (~kid=?, t: Token.t, rel: t): Result.t(Mold.t, option(Sort.o)) => {
  open Result.Syntax;
  let/ _ = mold_(~match=true, ~kid?, t, rel);
  mold_(~match=false, ~kid?, t, rel);
};

// let bounds = (rel: t): (Segment.Bound.t as 'b, 'b) => {
//   let bounds = Slopes.bounds(get_slopes(rel));
//   switch (bounds, Chain.unlink(rel)) {
//   | (_, None)
//   | ((Some(_), Some(_)), _) => bounds
//   | ((None, _) | (_, None), Some((_, (par_l, par_r), _))) =>
//     let (l, r) = bounds;
//     let l = Option.value(l, ~default=par_l);
//     let r = Option.value(r, ~default=par_r);
//     (Some(l), Some(r));
//   };
// };

let rec insert_terr = (~complement, terr: Terrace.L.t, rel: t): t => {
  let (p, rest) = Terrace.split_face(terr);
  switch (p.shape) {
  | G(g) =>
    // todo: may need to remold?
    rel
    |> cons(~onto=L, Terrace.of_piece(Piece.of_grout(g)))
    |> insert_up(~complement, Slope.Up.of_meld(rest))
  | T(t) =>
    switch (mold(t.token, rel)) {
    | Ok(m) when m == t.mold =>
      // todo: need to strengthen this fast check to include completeness check on kids
      // todo: convert to prefix form
      cons(~onto=L, terr, rel)
    | m =>
      let t =
        switch (m) {
        | Error(_) => t
        | Ok(mold) => {...t, mold}
        };
      rel
      |> cons(~onto=L, Terrace.of_piece(Piece.of_tile(t)))
      |> insert_up(~complement, Slope.Up.of_meld(rest));
    }
  };
}
and insert_complement = (rel: t) => {
  let (pre, _) = get_slopes(rel);
  Slope.Dn.complement(pre)
  |> List.map(((tok, mol)) =>
       Terrace.of_piece(Piece.of_grout(Grout.mk(~sugg=tok, mol)))
     )
  |> List.fold_left(Fun.flip(insert_terr(~complement=false)), rel);
}
and insert_space = (~complement, s: Space.t, rel: t) =>
  s.chars
  |> List.fold_left(
       (rel, s: Space.Char.t) =>
         rel
         |> (s.shape == Newline && complement ? insert_complement : Fun.id)
         |> cons_space(~onto=L, Space.mk([s])),
       rel,
     )
and insert_up = (~complement, up: Slope.Up.t, rel: t): t =>
  up
  |> Slope.Up.fold(
       s => insert_space(~complement, s, rel),
       (rel, t) => insert_terr(~complement, t, rel),
     );

let insert_lexeme = (~complement=false, lx: Lexeme.t, rel: t): t =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => insert_space(~complement, s, rel)
  | Ok(p) => insert_terr(~complement, Terrace.of_piece(p), rel)
  };

let regrout = rel => {
  let sib_of_g = g => Slopes.mk(~r=Segment.of_meld(Meld.of_grout(g)), ());
  let sib_of_p = (side: Dir.t, mel) =>
    switch (Option.get(Meld.tip(Dir.toggle(side), mel))) {
    | Convex => Slopes.empty
    | Concave(sort, _) => sib_of_g(Grout.mk_convex(sort))
    };
  let sib =
    switch (bounds(rel)) {
    | (None, None) => sib_of_g(Grout.mk_convex(Sort.root_o))
    | (None, Some(r)) => sib_of_p(R, r)
    | (Some(l), None) => sib_of_p(L, l)
    | (Some(l), Some(r)) =>
      // todo: maybe handle within-piece case
      switch (Meld.cmp(l, r)) {
      | In((sort, prec)) => sib_of_g(Grout.mk_concave(sort, prec))
      | Lt(_) => sib_of_p(R, r)
      | Eq(expected) => sib_of_g(Grout.mk_convex(expected.sort))
      | Gt(_) => sib_of_p(L, l)
      }
    };
  cons_slopes(sib, rel);
};

let rec remold_suffix = (rel: t): t => {
  let (pre, suf) = get_slopes(rel);
  switch (Chain.unlink(suf)) {
  | None => rel |> insert_complement |> regrout
  | Some((s, mel, suf)) =>
    rel
    |> put_slopes((pre, suf))
    |> insert_space(~complement=true, s)
    // note: insertion may flatten ancestors into siblings, in which
    // case additional elements may be added to suffix to be remolded
    // (safe bc rel is decreasing in height).
    |> insert_meld(~complement=true, mel)
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
  assert(s != "");
  let ((l, r), rel') = uncons_opt_lexemes(rel);
  switch (l, r) {
  | (Some(G(l)), Some(G(r))) when l.id == r.id =>
    let prefix = l.fill ++ s;
    switch (fill(prefix, r)) {
    | None => relex_insert(prefix, cons(~onto=R, Meld.of_grout(r), rel'))
    | Some(p) => (Lexed.empty, cons(~onto=L, Meld.of_piece(p), rel'))
    };
  | (_, Some(G(r))) when Option.is_some(fill(s, r)) =>
    let p = Option.get(fill(s, r));
    let rel =
      rel'
      |> cons_opt_lexeme(~onto=L, l)
      |> cons(~onto=L, Meld.of_piece(p))
      |> (Piece.is_grout(p) ? cons(~onto=R, Meld.of_grout(r)) : Fun.id);
    (Lexed.empty, rel);
  | _ =>
    // todo: recycle ids + avoid remolding if unaffected
    let tok_l = Option.(l |> map(Lexeme.token) |> value(~default=""));
    let tok_r = Option.(r |> map(Lexeme.token) |> value(~default=""));
    let lexed = (Lexer.lex(tok_l ++ s ++ tok_r), Token.length(tok_r));
    (lexed, rel');
  };
};
let relex = (~insert="", rel: t): (Lexed.t, t) =>
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

let insert = ((ls, offset): Lexed.t, rel: t): t => {
  let rel =
    switch (ls |> List.map(Lexeme.is_space) |> OptUtil.sequence) {
    | Some(s) =>
      rel
      |> cons_space(~onto=L, s)
      // remold if deletion, otherwise
      // fast path for space-only insertion
      |> (ls == [] ? remold_suffix : Fun.id)
    | None =>
      let inserted = List.fold_left(Fun.flip(insert_lexeme), rel, ls);
      print_endline("inserted = " ++ show(inserted));
      let ins_path = path(inserted);
      print_endline("ins_path = " ++ Meld.Path.show(ins_path));
      let remolded = remold_suffix(inserted);
      print_endline("remolded = " ++ show(remolded));
      let (zipped, _) = zip(remolded);
      unzip((zipped, ins_path));
    };
  FunUtil.(repeat(offset, force_opt(shift_char(~from=L)), rel));
};
