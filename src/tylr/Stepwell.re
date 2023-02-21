open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Slopes.t, Bridge.t);

// base slopes
let of_slopes: Slopes.t => t = Chain.of_loop;
let get_slopes: t => Slopes.t = Chain.fst;
let map_slopes: (_, t) => t = Chain.map_fst;
let put_slopes = sib => map_slopes(_ => sib);
let cons_slopes = (sib, rel) => map_slopes(Slopes.cat(sib), rel);

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
let rec cons_r = (mel: Terrace.R.t, ~kid=Meld.empty(), rel: t): t => {
  open Slope;
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
// let cons_meld = (~onto: Dir.t, mel, rel) =>
//   switch (onto) {
//   | L => rel |> cons_l(mel)
//   | R => rel |> cons_r(mel)
//   };
let cons_space = (~onto: Dir.t, s, rel) =>
  rel |> map_slopes(Slopes.cons_space(~onto, s));
// let cons_seg = (~onto: Dir.t, seg, rel) => {
//   let cons_space = cons_space(~onto);
//   let cons_meld = cons_meld(~onto);
//   switch (onto) {
//   | L =>
//     Segment.to_suffix(seg)
//     |> Chain.fold_left(
//          s => rel |> cons_space(s),
//          (rel, mel, s) => rel |> cons_meld(mel) |> cons_space(s),
//        )
//   | R =>
//     Segment.to_prefix(seg)
//     |> Chain.fold_right(
//          (s, mel, rel) => rel |> cons_meld(mel) |> cons_space(s),
//          s => rel |> cons_space(s),
//        )
//   };
// };
// let cons_parent = (par, rel) =>
//   switch (par) {
//   | _ when Bridge.is_empty(par) => rel
//   | (l, r) when Meld.is_empty(l) => cons_meld(~onto=R, r, rel)
//   | (l, r) when Meld.is_empty(r) => cons_meld(~onto=L, l, rel)
//   | _ => Chain.link(Slopes.empty, par, rel)
//   };

let assemble = (~sel=Segment.empty, rel: t): t => {
  let rec go = ((l, r) as sib, rel) =>
    switch (Chain.unlink(l), Chain.unknil(r)) {
    | (None, _)
    | (_, None) => cons_slopes(sib, rel)
    | (Some((s_l, mel_l, tl_l)), Some((tl_r, mel_r, s_r))) =>
      switch (Meld.cmp(mel_l, mel_r)) {
      | _ when Meld.(fst_id(mel_l) == lst_id(mel_r)) =>
        rel |> cons_slopes(sib)
      | In(_) =>
        rel
        |> cons_space(~onto=L, s_l)
        |> cons_space(~onto=R, s_r)
        |> cons_meld(~onto=L, mel_l)
        |> cons_meld(~onto=R, mel_r)
        |> go((tl_l, tl_r))
      | Eq(_) =>
        rel
        |> cons_space(~onto=L, s_l)
        |> cons_space(~onto=R, s_r)
        |> cons_parent((mel_l, mel_r))
        |> go((tl_l, tl_r))
      | Lt(_) =>
        rel
        |> cons_space(~onto=L, s_l)
        |> cons_meld(~onto=L, mel_l)
        |> go((tl_l, r))
      | Gt(_) =>
        rel
        |> cons_space(~onto=R, s_r)
        |> cons_meld(~onto=R, mel_r)
        |> go((l, tl_r))
      }
    };

  // let (pre, suf) = Slopes.assemble(get_slopes(rel));
  let (pre, suf) = get_slopes(rel);
  // separate siblings that belong to the selection
  let (pre_lt_sel, pre_geq_sel) = Segment.split_lt(pre, sel);
  let (sel_leq_suf, sel_gt_suf) = Segment.split_gt(sel, suf);
  rel
  |> put_slopes(Slopes.empty)
  |> go((pre_lt_sel, sel_gt_suf))
  |> cons_slopes((pre_geq_sel, sel_leq_suf));
};

let cons_lexeme = (~onto: Dir.t, lx: Lexeme.t) =>
  switch (lx) {
  | S(s) => cons_space(~onto, s)
  | G(_)
  | T(_) =>
    let terr = Terrace.of_piece(Option.get(Lexeme.to_piece(lx)));
    cons(~onto, terr);
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
    (~match, ~kid: option(Sort.o)=?, t: Token.t, rel: t): Mold.Result.t => {
  let rec go = (~kid: option(Sort.o)=?, rel: t): Mold.Result.t => {
    open Result.Syntax;
    let (pre, _) = get_slopes(rel);
    let/ kid = Segment.mold(~match, pre, ~kid?, t);
    switch (Chain.unlink(rel)) {
    | None =>
      match
        ? Error(kid)
        : Result.of_option(
            ~error=kid,
            LangUtil.mold_of_token(kid, Sort.root_o, t),
          )
    | Some((_slopes, par, rel)) =>
      let/ kid = Bridge.mold(~match, ~kid?, t, par);
      match ? go(~kid?, rel) : Error(kid);
    };
  };
  go(~kid?, rel);
};
let mold = (~kid=?, t: Token.t, rel: t): Mold.Result.t => {
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

// precond: mel is closed left
let rec insert_meld = (~complement, mel: Meld.t, rel: t): t => {
  let (p, rest) =
    Meld.is_closed_l(mel)
    |> OptUtil.get_or_raise(Invalid_argument("Stepwell.insert_meld"));
  switch (p.shape) {
  | G(g) =>
    // todo: may need to remold?
    rel
    |> cons_meld(~onto=L, Meld.of_grout(g))
    |> insert_seg(~complement, Segment.of_meld(rest))
  | T(t) =>
    switch (mold(t.token, rel)) {
    | Ok(m) when m == t.mold =>
      // todo: need to strengthen this fast check to include completeness check on kids
      // todo: convert to prefix form
      cons_meld(~onto=L, mel, rel)
    | m =>
      let t =
        switch (m) {
        | Error(_) => t
        | Ok(mold) => {...t, mold}
        };
      rel
      |> cons_meld(~onto=L, Meld.of_tile(t))
      |> insert_seg(~complement, Segment.of_meld(rest));
    }
  };
}
and insert_complement = (rel: t) => {
  let (pre, _) = get_slopes(rel);
  Segment.complement(~side=R, pre)
  |> List.map(((tok, mol)) => Meld.of_grout(Grout.mk(~sugg=tok, mol)))
  |> List.fold_left(Fun.flip(insert_meld(~complement=false)), rel);
}
and insert_space = (~complement, s: Space.s, rel: t) =>
  s
  |> List.fold_left(
       (rel, s: Space.t) =>
         rel
         |> (s.shape == Newline && complement ? insert_complement : Fun.id)
         |> cons_space(~onto=L, [s]),
       rel,
     )
and insert_seg = (~complement, seg: Segment.t, rel: t): t => {
  let ins_s = insert_space(~complement);
  let ins_mel = insert_meld(~complement);
  Segment.to_suffix(seg)
  |> Chain.fold_left(
       s => ins_s(s, rel),
       (rel, mel, s) => rel |> ins_mel(mel) |> ins_s(s),
     );
};

let insert_lexeme = (~complement=false, lx: Lexeme.t, rel: t): t =>
  switch (lx) {
  | S(s) => insert_space(~complement, [s], rel)
  | G(_)
  | T(_) =>
    let mel = Meld.of_piece(Option.get(Lexeme.to_piece(lx)));
    insert_meld(~complement, mel, rel);
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

let path = (rel: t): Meld.Path.t =>
  rel
  |> Chain.fold_right(
       (sib, par, path) =>
         path
         |> Meld.Path.cons(Bridge.step(par))
         |> Meld.Path.cons_s(Slopes.steps(sib)),
       Slopes.path,
     );

let zip = (rel: t): Meld.t =>
  rel
  |> Chain.fold_left(Slopes.zip_init, (zipped, par, sib) =>
       zipped |> Bridge.zip(par) |> Slopes.zip(sib)
     );
let unzip = (zipped: Meld.Zipped.t): t => {
  let invalid = Meld.Zipped.Invalid(zipped);
  let rec go = ((mel, path): Meld.Zipped.t, rel) => {
    let seg = Segment.(to_prefix(of_padded(mel)));
    go_seg((seg, path), rel);
  }
  and go_seg = ((seg, path), rel) =>
    switch (path.steps) {
    | [] =>
      rel
      |> cons_seg(~onto=R, seg)
      |> shift_chars(path.offset)
      |> OptUtil.get_or_raise(invalid)
    | [step, ...steps] =>
      switch (Chain.unlink(seg)) {
      | None => raise(invalid)
      | Some((s, mel, seg)) =>
        let (kid, par) = Bridge.unzip(step, mel);
        let rel = rel |> cons_space(~onto=L, s) |> cons_parent(par);
        switch (Meld.Padded.is_empty(kid)) {
        | None => go((kid, {...path, steps}), rel)
        | Some(s_kid) =>
          assert(Meld.is_empty(snd(par)));
          rel
          |> cons_space(~onto=L, s_kid)
          |> go_seg((seg, {...path, steps}));
        };
      }
    };
  go(zipped, empty);
};

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
    | None =>
      relex_insert(prefix, cons_meld(~onto=R, Meld.of_grout(r), rel'))
    | Some(p) => (Lexed.empty, cons_meld(~onto=L, Meld.of_piece(p), rel'))
    };
  | (_, Some(G(r))) when Option.is_some(fill(s, r)) =>
    let p = Option.get(fill(s, r));
    let rel =
      rel'
      |> cons_opt_lexeme(~onto=L, l)
      |> cons_meld(~onto=L, Meld.of_piece(p))
      |> (Piece.is_grout(p) ? cons_meld(~onto=R, Meld.of_grout(r)) : Fun.id);
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
