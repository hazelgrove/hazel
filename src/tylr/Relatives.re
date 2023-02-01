open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Siblings.t, Parent.t);

// immediate siblings
let of_sib: Siblings.t => t = Chain.of_loop;
let get_sib: t => Siblings.t = Chain.fst;
let map_sib: (_, t) => t = Chain.map_fst;
let put_sib = sib => map_sib(_ => sib);
let cons_sib = (sib, rel) => map_sib(Siblings.cat(sib), rel);

let empty = of_sib(Siblings.empty);

let cat: (t, t) => t = Chain.cat(Siblings.cat);
let concat = (rels: list(t)) => List.fold_right(cat, rels, empty);

let cons_parent = (par, rel) => Chain.link(Siblings.empty, par, rel);
let cons_space = (~onto: Dir.t, s, rel) =>
  rel |> map_sib(Siblings.cons_space(~onto, s));

let rec cons_meld_l = (~kid=Meld.Padded.empty(), mel: Meld.t, rel: t): t => {
  let (pre, suf) = get_sib(rel);
  switch (Segment.snoc_meld(pre, ~kid, mel)) {
  | In(pre)
  | Lt(pre)
  | Eq(pre) => put_sib((pre, suf), rel)
  | Gt(pre_kid) =>
    switch (Chain.unlink(rel)) {
    | None =>
      let pre = Segment.of_padded(Meld.(merge(pre_kid, Padded.mk(mel))));
      put_sib((pre, suf), rel);
    | Some((_sib, (par_l, par_r), rel)) =>
      switch (Meld.cmp_merge(par_l, ~kid=pre_kid, mel)) {
      | In(_) => raise(Parent.Convex_inner_tips)
      | Lt(kid_mel) => put_sib((Segment.of_padded(kid_mel), suf), rel)
      | Gt(par_l_kid) =>
        let r = Segment.(cat(suf, of_meld(par_r)));
        rel
        |> cons_sib(Siblings.mk(~r, ()))
        |> cons_meld_l(~kid=par_l_kid, mel);
      | Eq(par_l_mel) =>
        let l = Segment.of_padded(par_l_mel);
        let r = Segment.(cat(suf, of_meld(par_r)));
        rel |> cons_sib(Siblings.mk(~l, ~r, ()));
      }
    }
  };
};
let rec cons_meld_r = (mel: Meld.t, ~kid=Meld.Padded.empty(), rel: t): t => {
  let (pre, suf) = get_sib(rel);
  switch (Segment.cons_meld(mel, ~kid, suf)) {
  | In(suf)
  | Eq(suf)
  | Gt(suf) => put_sib((pre, suf), rel)
  | Lt(kid_suf) =>
    switch (Chain.unlink(rel)) {
    | None =>
      let suf = Segment.of_padded(Meld.(merge(Padded.mk(mel), kid_suf)));
      put_sib((pre, suf), rel);
    | Some((_sib, (par_l, par_r), rel)) =>
      switch (Meld.cmp_merge(mel, ~kid=kid_suf, par_r)) {
      | In(_) => raise(Parent.Convex_inner_tips)
      | Gt(mel_kid) => put_sib((pre, Segment.of_padded(mel_kid)), rel)
      | Lt(kid_par_r) =>
        let l = Segment.(cat(of_meld(par_l), pre));
        rel
        |> cons_sib(Siblings.mk(~l, ()))
        |> cons_meld_r(mel, ~kid=kid_par_r);
      | Eq(mel_par_r) =>
        let l = Segment.(cat(of_meld(par_l), pre));
        let r = Segment.of_padded(mel_par_r);
        rel |> cons_sib(Siblings.mk(~l, ~r, ()));
      }
    }
  };
};

let cons_meld = (~onto: Dir.t, mel, rel) =>
  switch (onto) {
  | L => rel |> cons_meld_l(mel)
  | R => rel |> cons_meld_r(mel)
  };

let cons_seg = (~onto: Dir.t, seg, rel) => {
  let cons_space = cons_space(~onto);
  let cons_meld = cons_meld(~onto);
  switch (onto) {
  | L =>
    Segment.to_suffix(seg)
    |> Chain.fold_left(
         s => rel |> cons_space(s),
         (rel, mel, s) => rel |> cons_meld(mel) |> cons_space(s),
       )
  | R =>
    Segment.to_prefix(seg)
    |> Chain.fold_right(
         (s, mel, rel) => rel |> cons_meld(mel) |> cons_space(s),
         s => rel |> cons_space(s),
       )
  };
};

let assemble = (~sel=Segment.empty, rel: t): t => {
  let rec go = ((l, r) as sib, rel) =>
    switch (Chain.unlink(l), Chain.unknil(r)) {
    | (None, _)
    | (_, None) => cons_sib(sib, rel)
    | (Some((s_l, mel_l, tl_l)), Some((tl_r, mel_r, s_r))) =>
      switch (Meld.cmp(mel_l, mel_r)) {
      | _ when Meld.(fst_id(mel_l) == lst_id(mel_r)) => rel |> cons_sib(sib)
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

  // let (pre, suf) = Siblings.assemble(get_sib(rel));
  let (pre, suf) = get_sib(rel);
  // separate siblings that belong to the selection
  let (pre_lt_sel, pre_geq_sel) = Segment.split_lt(pre, sel);
  let (sel_leq_suf, sel_gt_suf) = Segment.split_gt(sel, suf);
  rel
  |> put_sib(Siblings.empty)
  |> go((pre_lt_sel, sel_gt_suf))
  |> cons_sib((pre_geq_sel, sel_leq_suf));
};

let cons_lexeme = (~onto: Dir.t, lx: Lexeme.t) =>
  switch (lx) {
  | S(s) => cons_space(~onto, [s])
  | G(_)
  | T(_) =>
    let mel = Meld.of_piece(Option.get(Lexeme.to_piece(lx)));
    cons_meld(~onto, mel);
  };
let cons_opt_lexeme = (~onto: Dir.t, lx) =>
  switch (lx) {
  | None => Fun.id
  | Some(lx) => cons_lexeme(~onto, lx)
  };

let uncons = (~from_sib, ~from_par, ~from: Dir.t, rel: t) =>
  switch (from_sib(~from, get_sib(rel))) {
  | Some((a, sib)) => Some((a, put_sib(sib, rel)))
  | None =>
    open OptUtil.Syntax;
    let+ (sib, par, rel) = Chain.unlink(rel);
    let (a, par) = from_par(~from, par);
    let rel = rel |> cons_sib(Siblings.cat(sib, par)) |> assemble;
    (a, rel);
  };
let uncons_char =
  uncons(~from_sib=Siblings.uncons_char, ~from_par=Parent.uncons_char);
let uncons_lexeme =
  uncons(~from_sib=Siblings.uncons_lexeme, ~from_par=Parent.uncons_lexeme);
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

let rec zip = (~sel=Segment.empty, rel: t): Meld.Padded.t =>
  switch (Chain.unlink(rel)) {
  | None => Siblings.zip(~sel, get_sib(rel))
  | Some((sib, par, rel)) =>
    let kid = Siblings.zip(~sel, sib);
    let par = Parent.zip(kid, par);
    zip(~sel=Segment.of_padded(par), rel);
  };

let mold_ =
    (~match, ~kid: option(Sort.o)=?, t: Token.t, rel: t): Mold.Result.t => {
  let rec go = (~kid: option(Sort.o)=?, rel: t): Mold.Result.t => {
    open Result.Syntax;
    let (pre, _) = get_sib(rel);
    let/ kid = Segment.mold(~match, pre, ~kid?, t);
    switch (Chain.unlink(rel)) {
    | None =>
      match
        ? Error(kid)
        : Result.of_option(
            ~error=kid,
            LangUtil.mold_of_token(kid, Sort.root_o, t),
          )
    | Some((_sib, par, rel)) =>
      let/ kid = Parent.mold(~match, ~kid?, t, par);
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

let bounds = (rel: t): (Segment.Bound.t as 'b, 'b) => {
  let bounds = Siblings.bounds(get_sib(rel));
  switch (bounds, Chain.unlink(rel)) {
  | (_, None)
  | ((Some(_), Some(_)), _) => bounds
  | ((None, _) | (_, None), Some((_, (par_l, par_r), _))) =>
    let (l, r) = bounds;
    let l = Option.value(l, ~default=par_l);
    let r = Option.value(r, ~default=par_r);
    (Some(l), Some(r));
  };
};

// precond: mel is closed left
let rec insert_meld = (~complement, mel: Meld.t, rel: t): t => {
  let (p, rest) =
    Meld.is_closed_l(mel)
    |> OptUtil.get_or_raise(Invalid_argument("Relatives.insert_meld"));
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
  let (pre, _) = get_sib(rel);
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
  let sib_of_g = g => Siblings.mk(~r=Segment.of_meld(Meld.of_grout(g)), ());
  let sib_of_p = (side: Dir.t, mel) =>
    switch (Option.get(Meld.tip(Dir.toggle(side), mel))) {
    | Convex => Siblings.empty
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
  cons_sib(sib, rel);
};

let rec remold_suffix = (rel: t): t => {
  let (pre, suf) = get_sib(rel);
  switch (Chain.unlink(suf)) {
  | None => rel |> insert_complement |> regrout
  | Some((s, mel, suf)) =>
    rel
    |> put_sib((pre, suf))
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

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type rel = t;
  // overlapping coordinates at junctures
  // between spaces and melds
  [@deriving (show({with_path: false}), sexp, yojson)]
  type on_ =
    | Space(Space.Step.t)
    | Meld(Meld.Step.t);
  // top-down, depends only on prefix
  [@deriving (show({with_path: false}), sexp, yojson)]
  type steps = Chain.t(Siblings.Step.t, Parent.Step.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    steps,
    on_,
  };

  let of_ = (rel: rel) => {
    let (pre, suf) = get_sib(rel);
    let on_ =
      switch (Chain.(unknil(pre), unlink(suf))) {
      | (None, _)
      | (_, None)
      | (Some((_, _, [_, ..._])), Some(_))
      | (Some(_), Some(([_, ..._], _, _))) =>
        Space(Space.length(Chain.lst(pre)))
      | (Some((_, mel_l, [])), Some(([], mel_r, _))) =>
        switch (Meld.Step.of_((mel_l, mel_r))) {
        | None => Space(0)
        | Some(step) => Meld(step)
        }
      };
    let steps: steps =
      rel
      |> Chain.map(Siblings.Step.of_, Parent.Step.of_)
      |> Chain.rev(Fun.id, Fun.id);
    {steps, on_};
  };
};

let unzip = ({steps, on_}: Path.t, mel: Meld.Padded.t): t => {
  let rec go = (steps, kid: Meld.Padded.t, rel: t) => {
    let seg = Segment.(to_prefix(of_padded(kid)));
    switch (Chain.unlink(steps)) {
    | None =>
      let sib =
        switch (on_) {
        | Space(step) =>
          let (pre, s, suf) =
            Segment.split_nth_space(Chain.fst(steps), seg);
          let (s_l, s_r) = Space.unzip(step, s);
          Segment.(knil(pre, ~s=s_l, ()), link(~s=s_r, suf));
        | Meld(step) =>
          let (mel, (pre, suf)) = Siblings.unzip(Chain.fst(steps), seg);
          let (mel_l, mel_r) = Meld.unzip(step, mel);
          Siblings.cat(
            Segment.(of_padded(mel_l), of_padded(mel_r)),
            (pre, suf),
          );
        };
      cons_sib(sib, rel);
    | Some((step_sib, step_par, steps)) =>
      let (mel, sib) = Siblings.unzip(step_sib, seg);
      let (kid, par) = Parent.unzip(step_par, mel);
      let rel = rel |> map_sib(Siblings.cat(sib)) |> cons_parent(par);
      go(steps, kid, rel);
    };
  };
  go(steps, mel, empty);
};

module Lexed = {
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
      // fast path for deletion and space-only insertion
      rel |> cons_space(~onto=L, s) |> regrout
    | None =>
      let inserted = List.fold_left(Fun.flip(insert_lexeme), rel, ls);
      print_endline("inserted = " ++ show(inserted));
      let ins_path = Path.of_(inserted);
      print_endline("ins_path = " ++ Path.show(ins_path));
      remold_suffix(inserted) |> zip |> unzip(ins_path);
    };
  FunUtil.(repeat(offset, force_opt(shift_char(~from=L)), rel));
};
