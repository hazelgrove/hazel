type t = {
  sib: Siblings.t,
  anc: Ancestors.t,
  des: Descendants.t,
};

let mk = (~des=Descendants.empty, ~anc=Ancestors.empty, sib) => {
  sib, anc, des,
};

module Des = {
  type t = (Siblings.t, Lineage.t);

  let to_rel = ((sib, des): t) => mk(~des, sib)
}

module Outer = {
  // todo: probably don't need this, could just erase and mold
  // since we're not accumulating a chain
  let rec mold = (~match: bool, ~kid: option(Sort.t)=?, rel: t, t: Token.t): Mold.Result.t => {
    open Result.Syntax;
    let (pre, suf) = rel.sib;
    let/ kid = Segment.mold(~match, suf, t);
    let/ kid =
      switch (Descendants.pop(des)) {
      | None => Error(kid)
      | Some((par, _, _)) =>
        Parent.mold_outer(~match, par, ~kid?, t)
      };
    let/ kid = Segment.mold(~match, pre, ~kid?, t);
    switch (Ancestors.pop(rel.anc))   {
    | None => Error(kid)
    | Some((par, sib, anc)) =>
      let/ kid = Parent.mold_inner(~match, ~kid?, t, par);
      match
      ? mold(~match, ~kid?, t, mk(~sib, ~anc, ()))
      : Error(kid);
    };
  };

  let push_pre = (~kid=empty, c: Chain.t, rel: t): t => {
    let (pre, _) = rel.sib;

  }

  // gonna try an implementation pass that
  // - features comparison propagation (assume all such types already implemented)
  // - has no abstraction to other sub data structures like segment
  // actually, scratch the second one
  let push = (~kid=?, c: Chain.t, rel: t): t => {
    let (pre, suf) = rel.sib;
    switch (Segment.push(suf, ~kid?, c)) {
    | Lt(suf)
    | Eq(suf) => Ok({...rel, sib: (pre, suf)})
    // todo: revisit to add back space
    | Gt(kid) =>
      switch (Descendants.push(rel.des, ~kid?, c)) {
      | Lt(suf) => Ok({...rel, sib: (pre, suf)})
      | Eq(des) => Ok({...rel, des, sib: (pre, Segment.empty)})
      | Gt(des) =>
        switch (Segment.push_kid_des(pre, ~kid=des, c)) {
        | Lt((pre, des))
        | Eq((pre, des)) => Ok({...rel, des, sib: (pre, Segment.empty)})
        | Gt(des) =>
          switch (Ancestors.pop(rel.anc)) {
          | None => failwith("todo")
          | Some(((par_l, par_r), (sib_l, sib_r), anc)) =>
            switch (Chain.push_kid_des(par_l, ~kid=des, c)) {
            | Lt(des) => Ok({...rel, des, sib: Siblings.empty})
            | Eq(des) =>
              // wait... this suggests next pushed chain will be after sib_r...
              // but it should be after des...
              // maybe des and anc need their own sibs after all
              // dec suf parsed, anc suf are chains needing to be remolded
              Ok({anc, des, sib: (
                sib_l, Segment.(concat([of_chain(par_r), sib_r]))
              )})
            }
          }
        }
      }
    }
  }

  let push = (~kid=?, c: Chain.t, rel: t): t => {
    let (pre, suf) = rel.sib;
    switch (Segment.push(suf, ~kid?, c)) {
    | Ok(suf) => Ok({...rel, sib: (pre, sib)})
    | Error(kid) =>
      let sdes =
        (Siblings.mk(~suf=Segment.of_chain(kid), ()), rel.des);
      switch (Descendants.pop(rel.des)) {
      | None =>
      }
      // check descendants
      // if none, continue with pre
      // if some, push c against top parent
      // - success type: descendants
      // - failure type: descendants merged with kid
    }
  };

  let push_des = (pre: Segment.t, ~kid: option)

  // insert_seg

  // precond: c is closed left
  let rec insert_chain = (c: Chain.t, rel: t) => {
    let (pre, suf) = rel.sib;
    let (kid, p, rest) = Chain.pop_piece(c) |> OptUtil.get_or_raise(Chain.Missing_root);
    assert(kid == None);
    switch (p) {
    | G(_) => failwith("todo grout")
    | T(t) =>
      let mold = mold(t.token, rel);
      if (mold == t.mold) {
        // todo: need to strengthen this fast check to include completeness check on kids
        push(c, rel)
      } else {
        rel
        |> push(L, Chain.of_tile({...t, mold}))
        |> insert_seg(rest)
      };
    }
  };



  let remold_suffix = (rel: t): t => {
    let (suf, rel) = pop_suffix(rel);
    suf
    |> Aba.fold_left(
      s => Outer.push_space(s, rel),
      (rel, c, s) =>
        rel |> Outer.insert_chain(c) |> Outer.push_space(s),
    )
  };

}

type push_result('a) = Result.t('a, option(Chain.t));

module Inner = {
  let mold = (~match, ~kid: option(Sort.t)=?, t: Token.t, rel: t): Mold.Result.t => {
    let rec go = (~kid: option(Sort.t)=?, rel: t): Mold.Result.t => {
      open Result.Syntax;
      let (pre, suf) = rel.sib;
      let/ kid = Segment.mold(~match, pre, ~kid?, t);
      switch (Ancestors.pop(rel.anc)) {
      | None => Error(kid)
      | Some((par, sib, anc)) =>
        let/ kid = Parent.mold_inner(~match, ~kid?, t, par);
        match ? go(~match, ~kid?, t, {sib, anc}) : Error(kid);
      }
    };
    go(~kid?, unzip(rel));
  };

  // todo: generalize push direction
  // precond: c is closed left
  let rec push = (~kid: option(Chain.t)=?, c: Chain.t, rel: t): push_result(t) => {
    open Result.Syntax;
    let (pre, suf) = rel.sib;
    switch (Segment.push(pre, ~kid?, c)) {
    | Ok(pre) => Ok({...rel, sib: (pre, sib)})
    | Error(kid) =>
      // todo: consider root case
      switch (Ancestors.pop(rel.anc)) {
      | None => Error(kid)
      | Some(((par_l, par_r), (sib_l, sib_r), anc)) =>
        let extended_suf = () =>
          Segment.(concat([suf, of_chain(par_r), sib_r]));
        // todo: may need to put final siblings in pre/suf forms
        switch (
          Chain.comp_merge(par_l, ~kid?, c)
          |> OptUtil.get_or_fail("parent inner should be comparable")
        ) {
        | (Lt, kid__c) =>
          Ok({...rel, sib: (Segment.of_chain(kid__c), suf)});
        | (Gt, par_l__kid) =>
          push(~kid=par_l__kid, c, mk(~anc, ~sib=(sib_l, extended_suf()), ()));
        | (Eq, par_l__kid__c) =>
          // todo: review None case wrt desired grout behavior
          let pre = Segment.(concat([sib_l, of_chain(par_l__kid__c)]));
          Ok(mk(~anc, ~sib=(pre, extended_suf()), ()));
        }
      }
    }
  };

  // precond: c is closed left
  let rec insert_chain = (c: Chain.t, rel: t): t => {
    let (kid, p, rest) = Chain.pop_piece(c) |> OptUtil.get_or_raise(Chain.Missing_root);
    assert(kid == None);
    switch (p) {
    | G(_) => failwith("todo grout")
    | T(t) =>
      let mold = mold(t.token, rel);
      if (mold == t.mold) {
        // todo: need to strengthen this fast check to include completeness check on kids
        push(c, rel)
      } else {
        rel
        |> push(L, Chain.of_tile({...t, mold}))
        |> insert_seg(rest)
      };
    };
  }
  and insert_seg = (seg: Segment.t, rel: t): t =>
    Segment.to_suffix(seg)
    |> Aba.fold_left(
      s => insert_space(s, unzip(rel)),
      (rel, c, s) => rel |> insert_chain(c) |> insert_space(s),
    );
};

let insert = (seg: Segment.t, rel: t): t =>
  rel |> Inner.insert_seg(seg) |> remold_suffix;


let shift_char = (from: Dir.t, rel) => {
  open OptUtil.Syntax;
  let+ (c, rel) = pop_char(rel);
  push_char(Dir.toggle(from), c, rel);
};

let rec mold_matching = (t: Token.t, rel: t) =>
  switch (Siblings.mold_matching(t, rel.sib)) {
  | Some(m) => Some(m)
  | None =>
    Ancestors.pop(rel.anc)
    |> OptUtil.and_then(((((l, _) as a, _) as g, anc)) =>
      Ancestor.finished_container(a)
      // todo: move out of siblings
      ? Siblings.choose_matching(l, t)
      : mold_matching(t, {anc, sib: Generation.disassemble(g)})
    )
  };

// todo naming...
let mold = (t: Token.t, rel: t) =>
  switch (Siblings,mold(t, rel.sib)) {
  | Some(m) => Some(m)
  | None =>
    Ancestors.pop(rel.anc)
    |> OptUtil.and_then(((((l, _) as a, _) as g, anc)) =>
      Ancestor.finished_container(a)
      // todo: move out of siblings
      ? Siblings.choose_matching(l, t)
      : mold_matching(t, {anc, sib: Generation.disassemble(g)})
    )
  }


// let ( x|)
// insert =

// look up viable sorts on left side of relatives.
// viable means leq expected sort from some chain on left.
// left only because we want parse always to be consistent
// with a fresh left-to-right transcription.
// precond: inputs already lexed such that t is not part
// of a token in rel.
// returns, in addition to the assigned mold, the
// relatives produced by pushing the molded token onto rel,
// which is useful accumulator state to expose when molding
// a sequence of tokens to avoid redoing work.
let mold = (t: Token.t, rel: t): option(Mold.t) => {
  let rec go = (~in_l: option(Sort.t)=?, rel: t) => {
    let (pre, _) = rel.sib;
    switch (Aba.unsnoc(pre)) {
    | Some((pre, c, _)) =>
      let go_next = () =>
        go(~in_l=Chain.sort(c), {...rel, sib: (pre, suf)});
      switch (Chain.expected_sort(R, c)) {
      | None => go_next()
      | Some(out) =>
        switch (choose(in_l, out, t)) {
        | None => go_next()
        | Some(m) => Some(m)
        }
      };
    | None =>
      switch (Ancestors.pop(rel.anc)) {
      | Some((g, anc)) =>
        let sib = Generation.disassemble(g);
        go(kid_l, {sib, anc});
      | None => choose(kid_l, Sort.root, t)
      }
    };
  };
  go(rel);
};

// postcond: returned token empty if nothing to pop
let rec pop_adj_token = (d: Dir.t, rel: t): option((Token.t, t)) => {
  open OptUtil.Syntax;
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
      }
    | R =>
      let* (s, c, suf) = Aba.uncons(suf);
      !Space.is_empty(s)
      ? None
      : {
        let+ (t, c_rest) = Chain.pop_token(L, c);
        let suf = Segment.concat([c_rest, suf]);
        (t, {...rel, sib: (pre, suf)});
      }
    };
  };
};

let lex = (s: string, rel: t): (Aba.t(Space.t, Token.t), (int, int), t) => {
  let (l, rel) =
    pop_adj_token(L, rel)
    |> OptUtil.get(("", rel));
  let (r, rel) =
    pop_adj_token(R, rel)
    |> OptUtil.get(("", rel));
  let popped_len = Token.(length(l), length(r));
  (LangUtil.lex(l ++ s ++ r), popped_len, rel);
};

let empty: t = failwith("todo");

let push_descendant = ((a, sib): Generation.t, rel: t): t => {
  anc: [(a, rel.sib), ...rel.anc],
  sib: sib,
};

let push_descendants = List.fold_left(push_descendant);

let remold_seg = (seg: Segment.t, rel: t): (Segment.t, t) =>
  seg
  |> Aba.fold_left_map(
    s => (rel, s),
    (rel, c, s) => {
      let (seg, rel) = remold_chain(c, rel);
      (rel, seg, s);
    },
  )
  |> PairUtil.map_snd(Segment.join)
  |> PairUtil.flip;

let remold = (~sel=Segment.empty, rel: t): t => {
  let (pre, suf) = rel.sib;
  switch (Ancestors.pop(rel.anc)) {
  | None =>

  }
}


// precond: input sel + rel are lexed
let remold = (~sel=Segment.empty, rel: t): (Segment.t, t) => {
  // accumulate rel with each molded token
  // first fold_left_map over the sel
  let (sel, acc) = remold_seg(sel, rel);
  let (suf, acc) = remold_seg(suf, acc);
  let done = () => {
    let (pre, _) = rel.sib;
    (sel, {...rel, sib: (pre, suf)});
  };
  switch (rel.anc) {
  | [] => done()
  | [((a_l, a_r), sib), ...anc] =>
    switch (Segment.(assemble(concat([a_l, pre, sel, suf, a_r])))) {
    | (_, [_]) => done()
    | _ =>
      let rel = {
        let (pre, _) = acc.sib;
        let (_, suf) = sib;
        {anc, sib: (pre, Segment.concat([a_r, suf]))};
      };
      remold(rel);
    }
  }



  // then call remold_suf
  // then rezip original rel pre with new rel suf?
};