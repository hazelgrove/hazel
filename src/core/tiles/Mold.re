open Sexplib.Std;
// supports tiles that take different-sorted unichildren
// but for now the codebase assumes that tiles only take
// same-sorted unichildren
// TODO refactor nibs
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  out: Sort.t,
  in_: list(Sort.t),
  nibs: Nibs.t,
};

let flip_nibs = m => {...m, nibs: Nibs.flip(m.nibs)};

let mk_op = (out, in_) => {
  let n = Nib.{shape: Convex, sort: out};
  {out, in_, nibs: (n, n)};
};
let mk_pre = (p, out, in_) => {
  let l = Nib.{shape: Convex, sort: out};
  let r = Nib.{shape: Concave(p), sort: out};
  {out, in_, nibs: (l, r)};
};
let mk_post = (p, out, in_) => {
  let l = Nib.{shape: Concave(p), sort: out};
  let r = Nib.{shape: Convex, sort: out};
  {out, in_, nibs: (l, r)};
};
let mk_bin = (p, out, in_) => {
  let n = Nib.{shape: Concave(p), sort: out};
  {out, in_, nibs: (n, n)};
};

// forms where tips can be different than out sort
let mk_pre' = (p, out, sort_l, in_, sort_r) => {
  let l = Nib.{shape: Convex, sort: sort_l};
  let r = Nib.{shape: Concave(p), sort: sort_r};
  {out, in_, nibs: (l, r)};
};
let mk_post' = (p, out, sort_l, in_, sort_r) => {
  let l = Nib.{shape: Concave(p), sort: sort_l};
  let r = Nib.{shape: Convex, sort: sort_r};
  {out, in_, nibs: (l, r)};
};
let mk_bin' = (p, out, sort_l, in_, sort_r) => {
  let l = Nib.{shape: Concave(p), sort: sort_l};
  let r = Nib.{shape: Concave(p), sort: sort_r};
  {out, in_, nibs: (l, r)};
};

let nibs = (~index=?, mold: t): Nibs.t =>
  switch (index) {
  | None => mold.nibs
  | Some(i) =>
    let (l, r) = mold.nibs;
    let in_ = mold.in_;
    let l =
      i == 0 ? l : Nib.{shape: Shape.concave(), sort: List.nth(in_, i - 1)};
    let r =
      i == List.length(in_)
        ? r : Nib.{shape: Shape.concave(), sort: List.nth(in_, i)};
    (l, r);
  };

let nib_shapes = (~index=?, mold: t): Nibs.shapes => {
  let (nib_l, nib_r) = nibs(~index?, mold);
  (nib_l.shape, nib_r.shape);
};

module Map = {
  type mold = t;
  include Id.Map;
  type nonrec t = Id.Map.t(list(mold));
};

let of_grout: (Grout.t, Sort.t) => t =
  (g, sort) => {
    nibs:
      // TODO(d): revisit this when reformulating molds
      switch (g.shape) {
      | Convex =>
        let n = Nib.{shape: Convex, sort};
        (n, n);
      | Concave =>
        let n = Nib.{shape: Concave(Precedence.min), sort};
        (n, n);
      },
    out: sort,
    in_: [],
  };

let of_whitespace = (l: Nib.t) => {
  nibs: (Nib.flip(l), l),
  out: l.sort,
  in_: [],
};
