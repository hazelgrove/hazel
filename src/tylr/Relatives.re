open Util;

// todo: change this to Chain.t(Siblings.t, Parent.t)
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sib: Siblings.t,
  anc: Ancestors.t,
};

let mk = (~sib=Siblings.empty, ~anc=Ancestors.empty, ()) => {sib, anc};

let empty = mk();

// let cons = (~onto: Dir.t, s, mel, rel) => {
//   ...rel,
//   sib: Siblings.cons(~onto, s, mel, rel.sib),
// };
let cons_space = (~onto: Dir.t, s, rel) => {
  ...rel,
  sib: Siblings.cons_space(~onto, s, rel.sib),
};
let cons_meld = (~onto: Dir.t, mel, rel) => {
  ...rel,
  sib: Siblings.cons_meld(~onto, mel, rel.sib),
};
let cons_parent = (par, rel) => mk(~anc=[(par, rel.sib), ...rel.anc], ());

let append = (rel: t, rel': t) =>
  List.fold_right(
    ((par, sib), rel') => {
      let sib = Siblings.concat([sib, rel'.sib]);
      let anc = [(par, sib), ...rel'.anc];
      mk(~anc, ());
    },
    rel.anc,
    rel',
  );
let concat = (rels: list(t)) => List.fold_right(append, rels, empty);

[@warning "-27"]
let pop_char = (~from: Dir.t, rel: t) => failwith("todo pop_char");
[@warning "-27"]
let push_char = (~onto: Dir.t, mel, rel: t) => failwith("todo push_char");

let shift_char = (~from: Dir.t, rel) => {
  open OptUtil.Syntax;
  let+ (c, rel) = pop_char(~from, rel);
  push_char(~onto=Dir.toggle(from), c, rel);
};

let uncons_lexeme = (~from as d: Dir.t, rel: t): option((Lexeme.t, t)) => {
  let b = Dir.toggle(d);
  let (sib_d, sib_b) = Dir.order(d, rel.sib);
  switch (Segment.pop_lexeme(~from=b, sib_d)) {
  | Some((l, sib_d)) =>
    let sib = Dir.unorder(d, (sib_d, sib_b));
    Some((l, {...rel, sib}));
  | None =>
    open OptUtil.Syntax;
    let* (par, sib, anc) = Ancestors.pop(rel.anc);
    let (par_d, par_b) = Dir.order(d, par);
    let+ (l, par_d_rest) = Meld.pop_lexeme(~from=b, par_d);
    let sib =
      Siblings.concat([rel.sib, (par_d_rest, Segment.of_meld(par_b)), sib]);
    (l, {anc, sib});
  };
};

[@warning "-27"]
let cons_lexeme = (~onto: Dir.t, l: Lexeme.t, rel: t) =>
  failwith("todo push_lexeme");

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
  | (None, Some((l, rel))) => cons_lexeme(~onto, l, rel)
  | (Some(f), Some((l, rel))) =>
    switch (f(l, rel)) {
    | Some(rel) => Some(rel)
    | None => rel |> cons_lexeme(~onto, l) |> shift_lexeme(~from, ~until?)
    }
  };
};

let parent = (rel: t): option(Parent.t) =>
  Ancestors.pop(rel.anc) |> Option.map(((par, _, _)) => par);

let rec zip = (~sel=Segment.empty, rel: t): Meld.Padded.t =>
  switch (Ancestors.pop(rel.anc)) {
  | None => Siblings.zip(~sel, rel.sib)
  | Some(((l, r) as par, sib, anc)) =>
    let kid = Siblings.zip(~l, ~r, ~sel, rel.sib);
    let par = Parent.zip(kid, par);
    zip(~sel=Segment.of_padded(par), {sib, anc});
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
    let (pre, _) = rel.sib;
    let/ kid = Segment.mold(~match, pre, ~kid?, t);
    switch (Ancestors.pop(rel.anc)) {
    | None => Error(kid)
    | Some((par, sib, anc)) =>
      let/ kid = Parent.mold(~match, ~kid?, t, par);
      match ? go(~kid?, {sib, anc}) : Error(kid);
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
  let (pre, suf) = Siblings.assemble(rel.sib);
  let (pre_lt_sel, pre_geq_sel) = Segment.split_lt(pre, sel);
  let (sel_leq_suf, sel_gt_suf) = Segment.split_gt(sel, suf);

  let rec go = ((l, r) as sib, rel) =>
    switch (Chain.unlink(l), Chain.unknil(r)) {
    | (None, _)
    | (_, None) => concat([mk(~sib, ()), rel])
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

  let rel = go((pre_lt_sel, sel_gt_suf), mk(~anc=rel.anc, ()));
  concat([mk(~sib=(pre_geq_sel, sel_leq_suf), ()), rel]);
};

// todo: not remolded case
let rec push_meld_l =
        (~remolded as _=true, ~kid=Meld.Padded.empty(), mel: Meld.t, rel: t)
        : t => {
  let (pre, suf) = rel.sib;
  switch (Segment.hsup_meld(pre, ~kid, mel)) {
  | In(pre)
  | Lt(pre)
  | Eq(pre) => {...rel, sib: (pre, suf)}
  | Gt(kid) =>
    switch (Ancestors.pop(rel.anc)) {
    | None =>
      let pre = Segment.of_padded(Meld.(merge(kid, Padded.mk(mel))));
      {...rel, sib: (pre, suf)};
    | Some(((par_l, par_r), (sib_l, sib_r), anc)) =>
      switch (Meld.cmp_merge(par_l, ~kid, mel)) {
      | In(_) => raise(Parent.Convex_inner_tips)
      | Lt(kid_r) => {...rel, sib: (Segment.of_padded(kid_r), suf)}
      | Gt(kid_l) =>
        let suf = Segment.(concat([suf, of_meld(par_r), sib_r]));
        push_meld_l(~kid=kid_l, mel, {anc, sib: (sib_l, suf)});
      | Eq(par_l__c) =>
        let pre = Segment.(concat([sib_l, of_padded(par_l__c)]));
        let suf = Segment.(concat([suf, of_meld(par_r), sib_r]));
        {anc, sib: (pre, suf)};
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
  let (pre, suf) = rel.sib;
  switch (Chain.unlink(suf)) {
  | None => rel
  | Some((s, mel, suf)) =>
    {...rel, sib: (pre, suf)}
    |> cons_space(~onto=L, s)
    // note: insertion may flatten ancestors into siblings, in which
    // case additional elements may be added to suffix to be remolded
    |> insert_meld(mel)
    |> remold_suffix
  };
};

// postcond: returned token empty if nothing to pop
let rec pop_adj_token = (d: Dir.t, rel: t): option((Token.t, t)) => {
  OptUtil.Syntax.(
    if (Segment.is_empty(Dir.choose(d, rel.sib))) {
      let* ((par_l, par_r), (sib_l, sib_r), anc) = Ancestors.pop(rel.anc);
      let pre = Segment.(concat([sib_l, of_meld(par_l)]));
      let suf = Segment.(concat([of_meld(par_r), sib_r]));
      pop_adj_token(d, {anc, sib: (pre, suf)});
    } else {
      let (pre, suf) = rel.sib;
      switch (d) {
      | L =>
        let* (pre, mel, s) = Chain.unknil(pre);
        !Space.is_empty(s)
          ? None
          : {
            let+ (t, c_rest) = Meld.pop_token(~from=R, mel);
            let pre = Segment.concat([pre, c_rest]);
            (t, {...rel, sib: (pre, suf)});
          };
      | R =>
        let* (s, mel, suf) = Chain.unlink(suf);
        !Space.is_empty(s)
          ? None
          : {
            let+ (t, c_rest) = Meld.pop_token(~from=L, mel);
            let suf = Segment.concat([c_rest, suf]);
            (t, {...rel, sib: (pre, suf)});
          };
      };
    }
  );
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

let rec relex_insert = (s: string, rel: t): (list(Lexeme.t), t) => {
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
let relex = (~insert="", rel: t): (list(Lexeme.t), t) =>
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

let insert = (ls: list(Lexeme.t), rel: t): t =>
  switch (ls |> List.map(Lexeme.is_space) |> OptUtil.sequence) {
  | Some(s) => cons_space(~onto=L, s, rel)
  | None =>
    let seg = Segment.of_lexemes(ls);
    rel |> insert_seg(seg) |> remold_suffix |> unzip;
  };
