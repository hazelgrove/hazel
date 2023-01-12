// realizing this should probably be something like
//   anc: Lineage.t,
//   sib: Siblings.t,
//   des: Lineage.t,
// where
//   Lineage.t = Chain.Tl.t(Parent.t, Siblings.t)
// come back to this later
type t = (Descendants.t, Ancestors.t);

let remold_suffix = (rel: t) => {
  let (pre, suf) = rel.sib;
  let (s, c, suf) = Segment.uncons(suf);
  switch (c) {
  | None => rel
  | Some(c) =>
    let (des, rest) = Descendants.remold(des, c);



    switch (Descendants.uncons(rel.des)) {
    | None =>
      let (pre, rest) = Segment.
    }
  }
};

let remold = (des: Descendants.t, seg: Segment.t, anc: Ancestors.t): t => {
  let (s, c, seg) = Segment.uncons(seg);
  switch (c) {
  | None => (Descendants.snoc_space(s, des), anc)
  | Some(c) =>
    let (des, rest) = Descendants.remold(des, c);
    switch (Ancestors.pop_par(anc))
  }
}

module Des = {
  let remold = (des: t, seg: Segment.t): t => failwith("todo des remold");

  let is_single_parent = failwith("todo des length");
};

module Anc = {
  exception Missing_root;

  let empty = Aba.mk(Siblings.[empty, empty], [Parent.root]);

  let hd: t => Siblings.t = Aba.first;

  let uncons = (anc: t): (Siblings.t, Parent.t, t) =>
    Aba.uncons(anc)
    |> OptUtil.get_or_raise(Missing_root);

  let push_descendants = (des: t, anc: t): t => failwith("todo");

  // suffix left untouched
  let remold_seg = (seg: Segment.t, anc: t): t => {
    let ((sib_l, sib_r), (par_l, par_r), anc) = uncons(anc);
    let (l, unmolded) = Segment.(remold(cons_chain(par_l, sib_l), sel));
    if (Segment.is_empty(unmolded)) {

  }

  let remold = (~sel=Segment.empty, anc: t): t => {
    let ((sib_l, sib_r), (par_l, par_r), anc) = uncons(anc);
    let (l, unmolded) = Segment.(remold(cons_chain(par_l, sib_l), sel));
    if (Segment.is_empty(unmolded)) {
      let des = Aba.mk([(l, Segment.empty)], []);
      let des' = Des.remold(des, Segment.(snoc_chain(sib_r, par_r)));
      // todo: may need to strengthen check to make sure ends remained the same
      if (Des.is_single_parent(des)) {
        push_descendants(des, anc);
      }
    }
  }
};

let remold_seg = (seg: Segment.t, rel: t): (Segment.t, t) => {
  let (pre, suf) = hd(rel);
  let (pre, unmolded) = Segment.remold(pre, seg);
  switch (Aba.uncons(rel)) {
  | None =>
    let pre = Segment.remold_rest(pre, unmolded);
    Aba.mk([(pre, suf)], []);
  | Some((_, par, rel)) =>
  }
};
