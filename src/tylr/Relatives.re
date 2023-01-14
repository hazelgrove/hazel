open Util;

// todo: change this to Aba.t(Siblings.t, Parent.t)
type t = {
  sib: Siblings.t,
  anc: Ancestors.t,
};

let empty: t = failwith("todo");

[@warning "-27"]
let pop_char = (~from: Dir.t, rel: t) => failwith("todo pop_char");
[@warning "-27"]
let push_char = (~onto: Dir.t, c, rel: t) => failwith("todo push_char");

let shift_char = (~from: Dir.t, rel) => {
  open OptUtil.Syntax;
  let+ (c, rel) = pop_char(~from, rel);
  push_char(~onto=Dir.toggle(from), c, rel);
};

let pop_lexeme = (~from as d: Dir.t, rel: t): option((Lexeme.t, t)) => {
  let b = Dir.toggle(d);
  let (sib_d, sib_b) = Dir.order(d, rel.sib);
  switch (Segment.pop_lexeme(~from=b, sib_d)) {
  | Some((l, sib_d)) =>
    let sib = Dir.unorder(d, (sib_d, sib_b));
    Some((l, {...rel, sib}));
  | None =>
    open OptUtil.Syntax;
    let* ((par, sib), anc) = Ancestors.pop(rel.anc);
    let (par_d, par_b) = Dir.order(d, par);
    let+ (l, par_d_rest) = Chain.pop_lexeme(~from=b, par_d);
    let sib =
      Siblings.concat([
        rel.sib,
        (par_d_rest, Segment.of_chain(par_b)),
        sib,
      ]);
    (l, {anc, sib});
  };
};

[@warning "-27"]
let push_lexeme = (~onto: Dir.t, l: Lexeme.t, rel: t) =>
  failwith("todo push_lexeme");

// if until is None, attempt to shift a single spiece.
// if until is Some(f), shift spieces until f succeeds or until no spieces left to shift.
// note the latter case always returns Some.
let rec shift_lexeme =
        (~until: option((Lexeme.t, t) => option(t))=?, ~from: Dir.t, rel: t)
        : option(t) => {
  let onto = Dir.toggle(from);
  switch (until, pop_lexeme(~from, rel)) {
  | (None, None) => None
  | (Some(_), None) => Some(rel)
  | (None, Some((l, rel))) => push_lexeme(~onto, l, rel)
  | (Some(f), Some((l, rel))) =>
    switch (f(l, rel)) {
    | Some(rel) => Some(rel)
    | None => rel |> push_lexeme(~onto, l) |> shift_lexeme(~from, ~until?)
    }
  };
};

let unzip = (rel: t): t =>
  rel
  |> shift_lexeme(~from=L, ~until=(l, rel) =>
       OptUtil.Syntax.(
         switch (l) {
         | G(_) => None
         | T(t) =>
           let+ (l, r) = Tile.split_cursor(t);
           rel |> push_lexeme(~onto=L, T(l)) |> push_lexeme(~onto=R, T(r));
         | S(s) =>
           let+ (l, r) = Space.split_cursor(s);
           rel |> push_lexeme(~onto=L, S(l)) |> push_lexeme(~onto=R, S(r));
         }
       )
     )
  |> OptUtil.get_or_fail("unexpected shift_lexeme postcond");

// todo: add wrapper that checks for matching and unmatching molds
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

// type kid = [ | `None(Space.t) | `Some(Space.t, Chain.t, Space.t)];
// let segment_of_kid = _ => failwith("todo");

// todo: make sure space isn't getting lost given kid type
let rec push_chain = (~kid=Segment.empty, c: Chain.t, rel: t): t => {
  let (pre, suf) = rel.sib;
  // todo: review use of Cmp.Result here wrt desired grout behavior
  switch (Segment.push_chain(pre, ~kid, c)) {
  | Lt(pre)
  | Eq(pre) => {...rel, sib: (pre, suf)}
  | Gt(kid) =>
    switch (Ancestors.pop(rel.anc)) {
    | None =>
      let pre = Segment.finish_l(~kid, c);
      {...rel, sib: (pre, suf)};
    | Some(((par_l, par_r), (sib_l, sib_r), anc)) =>
      switch (
        Chain.cmp_merge(par_l, ~kid, c)
        |> OptUtil.get_or_fail("parent inner should always be comparable")
      ) {
      | Lt(kid_r) =>
        let pre = Segment.of_chain(kid_r);
        {...rel, sib: (pre, suf)};
      | Gt(kid_l) =>
        let pre = sib_l;
        let suf = Segment.(concat([suf, of_chain(par_r), sib_r]));
        let kid = Segment.of_chain(kid_l);
        push_chain(~kid, c, {anc, sib: (pre, suf)});
      | Eq(par_l__c) =>
        let pre = Segment.(concat([sib_l, of_chain(par_l__c)]));
        let suf = Segment.(concat([suf, of_chain(par_r), sib_r]));
        {anc, sib: (pre, suf)};
      }
    }
  };
};

[@warning "-27"]
let push_chain_r = (~kid=Segment.empty, c: Chain.t, rel: t) =>
  failwith("todo push_chain_r");

[@warning "-27"]
let push_space = (~onto: Dir.t, _, _) => failwith("todo push_space");

let push_seg = (~onto: Dir.t, seg: Segment.t, rel: t): t =>
  switch (onto) {
  | L =>
    Segment.to_suffix(seg)
    |> Aba.fold_left(
         s => push_space(~onto, s, rel),
         (rel, s, c) => push_chain(~kid=Segment.of_space(s), c, rel),
       )
  | R =>
    Segment.to_prefix(seg)
    |> Aba.fold_right(
         (c, s, rel) => push_chain_r(~kid=Segment.of_space(s), c, rel),
         s => push_space(~onto, s, rel),
       )
  };

// precond: c is closed left
let rec insert_chain = (~space=Space.empty, c: Chain.t, rel: t): t => {
  let (kid, p, rest) =
    Chain.uncons(c) |> OptUtil.get_or_raise(Chain.Missing_root);
  assert(kid == None);
  switch (p) {
  | Piece.G(_) => failwith("todo grout")
  | Piece.T(t) =>
    let kid = Segment.of_space(space);
    switch (mold(t.token, rel)) {
    | Ok(m) when Some(m) == t.mold =>
      // todo: need to strengthen this fast check to include completeness check on kids
      push_chain(~kid, c, rel)
    | m =>
      let mold = Result.to_option(m);
      rel
      |> push_chain(~kid, Chain.of_tile({...t, mold}))
      |> insert_seg(rest);
    };
  };
}
and insert_seg = (seg: Segment.t, rel: t): t =>
  Segment.to_suffix(seg)
  |> Aba.fold_left(
       s => push_space(~onto=L, s, rel),
       (rel, s, c) => insert_chain(~space=s, c, rel),
     );

// precond: rel contains Cursor token in prefix
let rec remold_suffix = (rel: t): t => {
  let (pre, suf) = rel.sib;
  switch (Aba.uncons(suf)) {
  | None => rel
  | Some((s, c, suf)) =>
    {...rel, sib: (pre, suf)}
    // note: insertion may flatten ancestors into siblings,
    // in which case additional elements may be added to suffix
    |> insert_chain(~space=s, c)
    |> remold_suffix
  };
};

let insert = (seg: Segment.t, rel: t): t =>
  rel |> insert_seg(seg) |> remold_suffix;

// postcond: returned token empty if nothing to pop
let rec pop_adj_token = (d: Dir.t, rel: t): option((Token.t, t)) => {
  OptUtil.Syntax.(
    if (Segment.is_empty(Dir.choose(d, rel.sib))) {
      let* ((par_l, par_r), (sib_l, sib_r), anc) = Ancestors.pop(rel.anc);
      let pre = Segment.(concat([sib_l, of_chain(par_l)]));
      let suf = Segment.(concat([of_chain(par_r), sib_r]));
      pop_adj_token(d, {anc, sib: (pre, suf)});
    } else {
      let (pre, suf) = rel.sib;
      switch (d) {
      | L =>
        let* (pre, c, s) = Aba.unsnoc(pre);
        !Space.is_empty(s)
          ? None
          : {
            let+ (t, c_rest) = Chain.pop_token(~from=R, c);
            let pre = Segment.concat([pre, c_rest]);
            (t, {...rel, sib: (pre, suf)});
          };
      | R =>
        let* (s, c, suf) = Aba.uncons(suf);
        !Space.is_empty(s)
          ? None
          : {
            let+ (t, c_rest) = Chain.pop_token(~from=L, c);
            let suf = Segment.concat([c_rest, suf]);
            (t, {...rel, sib: (pre, suf)});
          };
      };
    }
  );
};

let lex = (s: string, rel: t): (list(Lexeme.t), t) => {
  let (l, rel) = pop_adj_token(L, rel) |> OptUtil.get(() => ("", rel));
  let (r, rel) = pop_adj_token(R, rel) |> OptUtil.get(() => ("", rel));

  let buf = Lexing.from_string(l ++ s ++ r);
  let rev = ref([]);
  while (!buf.lex_eof_reached) {
    let lx = Lexer.next_lexeme(buf);
    rev := [lx, ...rev^];
  };

  // todo: insert cursor sentinel
  (List.rev(rev^), rel);
};
