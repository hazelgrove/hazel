open Util;

// todo: change this to Chain.t(Siblings.t, Parent.t)
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Siblings.t, Parent.t);

// immediate siblings
let of_sib: Siblings.t => t = Chain.of_loop;
let get_sib: t => Siblings.t = Chain.fst;
let map_sib: (_, t) => t = Chain.map_fst;
let put_sib = sib => map_sib(_ => sib);

let empty = of_sib(Siblings.empty);

let cat: (t, t) => t = Chain.cat(Siblings.cat);
let concat = (rels: list(t)) => List.fold_right(cat, rels, empty);

let cons = (~onto_sib, ~onto: Dir.t, a, rel) =>
  map_sib(onto_sib(~onto, a), rel);
let cons_space = cons(~onto_sib=Siblings.cons_space);
let cons_meld = cons(~onto_sib=Siblings.cons_meld);
let cons_lexeme = cons(~onto_sib=Siblings.cons_lexeme);
let cons_parent = (par, rel) => Chain.link(Siblings.empty, par, rel);

let uncons = (~from_sib, ~from_par, ~from: Dir.t, rel: t) =>
  switch (from_sib(~from, get_sib(rel))) {
  | Some((a, sib)) => Some((a, put_sib(sib, rel)))
  | None =>
    open OptUtil.Syntax;
    let* (sib, par, rel) = Chain.unlink(rel);
    let+ (a, par) = from_par(~from, par);
    (a, map_sib(Siblings.(cat(cat(sib, par))), rel));
  };
let uncons_lexeme =
  uncons(~from_sib=Siblings.uncons_lexeme, ~from_par=Parent.uncons_lexeme);
let uncons_char =
  uncons(~from_sib=Siblings.uncons_char, ~from_par=Parent.uncons_char);

let shift_char = (~from: Dir.t, rel) => {
  open OptUtil.Syntax;
  let+ (c, rel) = uncons_char(~from, rel);
  cons_lexeme(~onto=Dir.toggle(from), c, rel);
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

// let parent = (rel: t): option(Parent.t) =>
//   Chain.unlink(rel) |> Option.map(((par, _, _)) => par);

let rec zip = (~sel=Segment.empty, rel: t): Meld.Padded.t =>
  switch (Chain.unlink(rel)) {
  | None => Siblings.zip(~sel, get_sib(rel))
  | Some((sib, par, rel)) =>
    let kid = Siblings.zip(~sel, sib);
    let par = Parent.zip(kid, par);
    zip(~sel=Segment.of_padded(par), rel);
  };

let unzip = (rel: t): t =>
  rel
  |> shift_lexeme(~from=L, ~until=(l, rel) =>
       OptUtil.Syntax.(
         switch (l) {
         | G(_) => None
         | T(t) =>
           let+ (l, r) = Tile.split_cursor(t);
           rel |> cons_lexeme(~onto=L, T(l)) |> cons_lexeme(~onto=R, T(r));
         | S(s) => Space.is_cursor(s) ? Some(rel) : None
         }
       )
     )
  |> OptUtil.get_or_fail("unexpected shift_lexeme postcond");

let mold_ =
    (~match, ~kid: option(Sort.t)=?, t: Token.t, rel: t): Mold.Result.t => {
  let rec go = (~kid: option(Sort.t)=?, rel: t): Mold.Result.t => {
    open Result.Syntax;
    let (pre, _) = get_sib(rel);
    let/ kid = Segment.mold(~match, pre, ~kid?, t);
    switch (Chain.unlink(rel)) {
    | None => Error(kid)
    | Some((_sib, par, rel)) =>
      let/ kid = Parent.mold(~match, ~kid?, t, par);
      match ? go(~kid?, rel) : Error(kid);
    };
  };
  go(~kid?, unzip(rel));
};
let mold = (~kid=?, t: Token.t, rel: t): Mold.Result.t => {
  open Result.Syntax;
  let/ _ = mold_(~match=true, ~kid?, t, rel);
  mold_(~match=false, ~kid?, t, rel);
};

let assemble = (~sel=Segment.empty, rel: t): t => {
  let rec go = ((l, r) as sib, rel) =>
    switch (Chain.unlink(l), Chain.unknil(r)) {
    | (None, _)
    | (_, None) => map_sib(Siblings.cat(sib), rel)
    | (Some((s_l, c_l, tl_l)), Some((tl_r, c_r, s_r))) =>
      switch (Meld.cmp(c_l, c_r)) {
      | In () => raise(Segment.Disconnected)
      | Lt () =>
        rel
        |> cons_space(~onto=L, s_l)
        |> cons_meld(~onto=L, c_l)
        |> go((tl_l, r))
      | Eq () =>
        rel
        |> cons_space(~onto=L, s_l)
        |> cons_space(~onto=R, s_r)
        |> cons_parent((c_l, c_r))
        |> go((tl_l, tl_r))
      | Gt () =>
        rel
        |> cons_space(~onto=R, s_r)
        |> cons_meld(~onto=R, c_r)
        |> go((r, tl_r))
      }
    };

  let (pre, suf) = Siblings.assemble(get_sib(rel));
  // separate siblings that belong to the selection
  let (pre_lt_sel, pre_geq_sel) = Segment.split_lt(pre, sel);
  let (sel_leq_suf, sel_gt_suf) = Segment.split_gt(sel, suf);
  rel
  |> put_sib(Siblings.empty)
  |> go((pre_lt_sel, sel_gt_suf))
  |> map_sib(Siblings.cat((pre_geq_sel, sel_leq_suf)));
};

// todo: not remolded case
let rec push_meld_l =
        (~remolded as _=true, ~kid=Meld.Padded.empty(), mel: Meld.t, rel: t)
        : t => {
  let (pre, suf) = get_sib(rel);
  switch (Segment.hsup_meld(pre, ~kid, mel)) {
  | In(pre)
  | Lt(pre)
  | Eq(pre) => put_sib((pre, suf), rel)
  | Gt(kid) =>
    switch (Chain.unlink(rel)) {
    | None =>
      let pre = Segment.of_padded(Meld.(merge(kid, Padded.mk(mel))));
      put_sib((pre, suf), rel);
    | Some((_sib, (par_l, par_r), rel)) =>
      switch (Meld.cmp_merge(par_l, ~kid, mel)) {
      | In(_) => raise(Parent.Convex_inner_tips)
      | Lt(kid) => put_sib((Segment.of_padded(kid), suf), rel)
      | Gt(kid) =>
        rel
        |> map_sib(
             Siblings.cat(Segment.(empty, cat(suf, of_meld(par_r)))),
           )
        |> push_meld_l(~kid, mel)
      | Eq(par_l__c) =>
        rel
        |> map_sib(
             Siblings.cat(
               Segment.(of_padded(par_l__c), cat(suf, of_meld(par_r))),
             ),
           )
      }
    }
  };
};

// precond: mel is closed left
let rec insert_meld = (mel: Meld.t, rel: t): t => {
  let (kid, p, rest) =
    Chain.unlink(mel) |> OptUtil.get_or_raise(Meld.Missing_root);
  assert(kid == None);
  switch (p.shape) {
  | G(_) => rel |> insert_seg(Segment.(to_suffix(of_meld(rest))))
  | T(t) =>
    switch (mold(t.token, rel)) {
    | Ok(m) when m == t.mold =>
      // todo: need to strengthen this fast check to include completeness check on kids
      push_meld_l(mel, rel)
    | m =>
      let t =
        switch (m) {
        | Error(_) => t
        | Ok(mold) => {...t, mold}
        };
      rel
      |> push_meld_l(Meld.of_tile(t))
      |> insert_seg(Segment.(to_suffix(of_meld(rest))));
    }
  };
}
and insert_seg = (seg: Segment.t, rel: t): t =>
  Segment.to_suffix(seg)
  |> Chain.fold_left(
       s => cons_space(~onto=L, s, rel),
       (rel, mel, s) => rel |> insert_meld(mel) |> cons_space(~onto=L, s),
     );

// precond: rel contains Cursor token in prefix
let rec remold_suffix = (rel: t): t => {
  let (pre, suf) = get_sib(rel);
  switch (Chain.unlink(suf)) {
  | None => rel
  | Some((s, mel, suf)) =>
    rel
    |> put_sib((pre, suf))
    |> cons_space(~onto=L, s)
    // note: insertion may flatten ancestors into siblings, in which
    // case additional elements may be added to suffix to be remolded
    |> insert_meld(mel)
    |> remold_suffix
  };
};

let cons_opt_lexeme = (~onto: Dir.t, l, rel) =>
  switch (l) {
  | None => rel
  | Some(l) => cons_lexeme(~onto, l, rel)
  };

let uncons_opt_lexemes = (rel: t): ((option(Lexeme.t) as 'l, 'l), t) => {
  let (l, rel) =
    switch (uncons_lexeme(~from=L, rel)) {
    | None => (None, rel)
    | Some((l, rel)) => (Some(l), rel)
    };
  let (r, rel) =
    switch (uncons_lexeme(~from=R, rel)) {
    | None => (None, rel)
    | Some((r, rel)) => (Some(r), rel)
    };
  ((l, r), rel);
};

let fill = (s: string, g: Grout.t): option(Piece.t) =>
  if (String.equal(s, Grout.suggestion(g))) {
    Some(Piece.mk(T(Tile.mk(~id=g.id, g.mold, s))));
  } else if (String.starts_with(~prefix=s, Grout.suggestion(g))) {
    Some(Piece.mk(G(Grout.mk(~id=g.id, ~prefix=s, g.mold))));
  } else {
    None;
  };

let rec relex_insert = (s: string, rel: t): (Lexeme.s, t) => {
  assert(s != "");
  let ((l, r), rel') = uncons_opt_lexemes(rel);
  switch (l, r) {
  | (Some(G(l)), Some(G(r))) when l.id == r.id =>
    let prefix = l.prefix ++ s;
    switch (fill(prefix, r)) {
    | None =>
      relex_insert(prefix, cons_meld(~onto=R, Meld.of_grout(r), rel'))
    | Some(p) => ([], cons_meld(~onto=L, Meld.of_piece(p), rel'))
    };
  | (_, Some(G(r))) when Option.is_some(fill(s, r)) =>
    let p = Option.get(fill(s, r));
    let rel =
      rel'
      |> cons_opt_lexeme(~onto=L, l)
      |> cons_meld(~onto=L, Meld.of_piece(p))
      |> (Piece.is_grout(p) ? cons_meld(~onto=R, Meld.of_grout(r)) : Fun.id);
    ([], rel);
  | _ =>
    // todo: recycle ids + avoid remolding if unaffected
    let tok_l = Option.(l |> map(Lexeme.token) |> value(~default=""));
    let tok_r = Option.(r |> map(Lexeme.token) |> value(~default=""));
    (Lexer.lex(tok_l ++ s ++ tok_r), rel');
  };
};
let relex = (~insert="", rel: t): (Lexeme.s, t) =>
  switch (insert) {
  | "" =>
    let ((l, r), rel') = uncons_opt_lexemes(rel);
    switch (l, r) {
    | (None | Some(S(_) | G(_)), _)
    | (_, None | Some(S(_) | G(_))) => ([], rel)
    | (Some(T(l)), Some(T(r))) =>
      switch (Lexer.lex(l.token ++ r.token)) {
      | [T(l'), T(r')] when l'.token == l.token && r'.token == r.token => (
          [],
          rel,
        )
      | ls => (ls, rel')
      }
    };
  | _ => relex_insert(insert, rel)
  };

let insert = (ls: Lexeme.s, rel: t): t =>
  switch (ls |> List.map(Lexeme.is_space) |> OptUtil.sequence) {
  | Some(s) => cons_space(~onto=L, s, rel)
  | None =>
    let seg = Segment.of_lexemes(ls);
    rel |> insert_seg(seg) |> remold_suffix |> unzip;
  };
