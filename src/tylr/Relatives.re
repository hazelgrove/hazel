// todo: change this to Aba.t(Siblings.t, Parent.t)
type t = {
  sib: Siblings.t,
  anc: Ancestors.t,
};

let empty: t = failwith("todo");

let shift_char = (~from: Dir.t, rel) => {
  open OptUtil.Syntax;
  let+ (c, rel) = pop_char(~from, rel);
  push_char(~onto=Dir.toggle(from), c, rel);
};

let pop_spiece = (~from as d: Dir.t, rel: t): option((Spiece.t, t)) => {
  let b = Dir.toggle(d);
  let (sib_d, sib_b) = Dir.order(d, rel.sib);
  switch (Segment.pop_spiece(~from=b, sib_d)) {
  | Some((sp, sib_d)) =>
    let sib = Dir.unorder(d, (sib_d, sib_b));
    Some((sp, {...rel, sib}));
  | None =>
    open OptUtil.Syntax;
    let* ((par, sib), anc) = Ancestors.pop(rel.anc);
    let (par_d, par_b) = Dir.order(d, par);
    let+ (p, par_d_rest) = Chain.pop_piece(~from=b, par_d);
    let sib =
      Siblings.concat([
        rel.sib,
        (par_d_rest, Segment.of_chain(par_d)),
        sib,
      ]);
    (P(p), {anc, sib});
  };
};

// if until is None, attempt to shift a single spiece.
// if until is Some(f), shift spieces until f succeeds or until no spieces left to shift.
// note the latter case always returns Some.
let rec shift_spiece =
        (~until: option((Spiece.t, t) => option(t))=?, ~from: Dir.t, rel: t)
        : option(t) => {
  let onto = Dir.toggle(from);
  switch (until, pop_spiece(rel)) {
  | (None, None) => None
  | (Some(_), None) => Some(rel)
  | (None, Some((sp, rel))) => push_spiece(~onto, sp, rel)
  | (Some(f), Some((sp, rel)) as r) =>
    switch (f(sp, rel)) {
    | Some(rel) => Some(rel)
    | None => rel |> push_spiece(~onto, sp) |> shift_spiece(~from, ~until?)
    }
  };
};

let unzip = (rel: t): t =>
  rel
  |> shift_spiece(~from=L, ~until=(sp, rel) =>
       switch (sp) {
       | P(_) => None
       | S(s) =>
         open OptUtil.Syntax;
         let+ (l, r) = Space.split_cursor(s);
         rel |> push_space(~onto=L, l) |> push_space(~onto=R, r);
       }
     );

// todo: add wrapper that checks for matching and unmatching molds
let mold =
    (~match, ~kid: option(Sort.t)=?, t: Token.t, rel: t): Mold.Result.t => {
  let rec go = (~kid: option(Sort.t)=?, rel: t): Mold.Result.t => {
    open Result.Syntax;
    let (pre, suf) = rel.sib;
    let/ kid = Segment.mold(~match, pre, ~kid?, t);
    switch (Ancestors.pop(rel.anc)) {
    | None => Error(kid)
    | Some((par, sib, anc)) =>
      let/ kid = Parent.mold(~match, ~kid?, t, par);
      match ? go(~match, ~kid?, t, {sib, anc}) : Error(kid);
    };
  };
  go(~kid?, unzip(rel));
};

type kid = [ | `None(Space.t) | `Some(Space.t, Chain.t, Space.t)];

let segment_of_kid = _ => failwith("todo");

let push_chain = (~kid: kid=`None(Space.empty), c: Chain.t, rel: t): t => {
  let (pre, suf) = rel.sib;
  // todo: review use of Cmp.Result here wrt desired grout behavior
  switch (Segment.push(pre, ~kid, c)) {
  | Lt(pre)
  | Eq(pre) => {...rel, sib: (pre, suf)}
  | Gt(kid) =>
    switch (Ancestors.pop(rel.anc)) {
    | None =>
      let pre = Segment.of_chain(Chain.finish_l(~kid, c));
      {...rel, sib: (pre, suf)};
    | Some(((par_l, par_r), (sib_l, sib_r), anc)) =>
      switch (Chain.cmp_merge(par_l, ~kid, c)) {
      | Lt(kid_r) =>
        let pre = segment_of_kid(kid_r);
        {...rel, sib: (pre, suf)};
      | Gt(kid_l) =>
        let pre = sib_l;
        let suf = Segment.(concat([suf, of_chain(par_r), sib_r]));
        push_chain(~kid, c, {anc, sib: (pre, suf)});
      | Eq(c') =>
        let pre = Segment.(concat([sib_l, of_chain(c')]));
        let suf = Segment.(concat([suf, of_chain(par_r), sib_r]));
        {anc, sib: (pre, suf)};
      }
    }
  };
};

// precond: c is closed left
let rec insert_chain = (~space=Space.empty, c: Chain.t, rel: t): t => {
  let (kid, p, rest) =
    Chain.pop_piece(c) |> OptUtil.get_or_raise(Chain.Missing_root);
  assert(kid == None);
  switch (p) {
  | G(_) => failwith("todo grout")
  | T(t) =>
    let mold = mold(t.token, rel);
    if (mold == t.mold) {
      // todo: need to strengthen this fast check to include completeness check on kids
      push_chain(
        ~space,
        c,
        rel,
      );
    } else {
      rel
      |> push_chain(~space, Chain.of_tile({...t, mold}))
      |> insert_seg(rest);
    };
  };
}
and insert_seg = (seg: Segment.t, rel: t): t =>
  Segment.to_suffix(seg)
  |> Aba.fold_left(
       s => insert_space(s, rel),
       (rel, s, c) => insert_chain(~space=s, c),
     );

// precond: rel contains Cursor token in prefix
let rec remold_suffix = (rel: t): t => {
  let (pre, suf) = rel.sib;
  switch (Aba.uncons(suf)) {
  | None => rel
  | Some((s, c, suf)) =>
    {...rel, sib: (pre, suf)}
    |> insert_chain(~kid=`None(s), c)
    |> remold_suffix
  };
};

let insert = (seg: Segment.t, rel: t): t =>
  rel |> insert_seg(seg) |> remold_suffix;

// postcond: returned token empty if nothing to pop
let rec pop_adj_token = (d: Dir.t, rel: t): option((Token.t, t)) => {
  OptUtil.Syntax.(
    if (Segment.is_empty(Dir.choose(d, rel.sib))) {
      let* ((par_l, par_r), (sib_l, sib_r), anc) =
        Ancestors.pop_up_to_root(rel.anc);
      let pre = Segment.(concat([sib_l, of_chain(par_l)]));
      let suf = Segment.(concat([of_chain(par_r), sib_r]));
      pop_adj_token(d, {anc, sib: (pre, suf)});
    } else {
      switch (d) {
      | L =>
        let* (pre, c, s) = Aba.unsnoc(pre);
        !Space.is_empty(s)
          ? None
          : {
            let+ (t, c_rest) = Chain.pop_token(R, c);
            let pre = Segment.concat([pre, c_rest]);
            (t, {...rel, sib: (pre, suf)});
          };
      | R =>
        let* (s, c, suf) = Aba.uncons(suf);
        !Space.is_empty(s)
          ? None
          : {
            let+ (t, c_rest) = Chain.pop_token(L, c);
            let suf = Segment.concat([c_rest, suf]);
            (t, {...rel, sib: (pre, suf)});
          };
      };
    }
  );
};

let lex = (s: string, rel: t): (Segment.t, t) => {
  let (l, rel) = pop_adj_token(L, rel) |> OptUtil.get(("", rel));
  let (r, rel) = pop_adj_token(R, rel) |> OptUtil.get(("", rel));

  let buf = Lexing.from_string(s);
  let rev_seg = ref(Segment.empty);
  while (!buf.lex_eof_reached) {
    let sp = Lexer.next_spiece(buf);
    rev_seg := Segment.cons_spiece(sp, rev_seg);
  };

  let seg = Aba.rev(Fun.id, Fun.id, rev_seg);
  // todo: insert cursor sentinel
  (seg, rel);
};
