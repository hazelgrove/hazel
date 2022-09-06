open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type child = {
  sort: Sort.t,
  pad: (bool, bool),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  out: Sort.t,
  in_: list(child),
  nibs: Nibs.t,
};

let flip_nibs = m => {...m, nibs: Nibs.flip(m.nibs)};

let mk_op = (out, in_) => {
  let n = Nib.{shape: Convex, sort: out};
  {out, in_, nibs: (n, n)};
};
let mk_pre = (~pad=false, prec, out, in_) => {
  let l = Nib.{shape: Convex, sort: out};
  let r = Nib.{shape: Shape.concave(~prec, ~pad, ()), sort: out};
  {out, in_, nibs: (l, r)};
};
let mk_post = (~pad=false, prec, out, in_) => {
  let l = Nib.{shape: Shape.concave(~prec, ~pad, ()), sort: out};
  let r = Nib.{shape: Convex, sort: out};
  {out, in_, nibs: (l, r)};
};
// TODO clean up api
let mk_bin = (~pad_l=false, ~pad_r=false, ~l=?, ~r=?, prec, out, in_) => {
  let l = Option.value(l, ~default=out);
  let r = Option.value(r, ~default=out);
  let nib = (pad, sort) => Nib.{sort, shape: Concave({prec, pad})};
  {out, in_, nibs: (nib(pad_l, l), nib(pad_r, r))};
};

let mk_child = (~pad_l=false, ~pad_r=false, sort) => {
  sort,
  pad: (pad_l, pad_r),
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
    let l =
      i == 0
        ? l
        : {
          let {sort, pad: (_, pad)} = List.nth(mold.in_, i - 1);
          Nib.{sort, shape: Shape.concave(~pad, ())};
        };
    let r =
      i == List.length(mold.in_)
        ? r
        : {
          let {sort, pad: (pad, _)} = List.nth(mold.in_, i);
          Nib.{sort, shape: Shape.concave(~pad, ())};
        };
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
        let n = Nib.{shape: Shape.concave(), sort};
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

let fits_shape = (d: Direction.t, s: Nib.Shape.t, m: t): bool => {
  let s' = Direction.choose(d, nib_shapes(m));
  Nib.Shape.fits(s, s');
};

let consistent_shapes = (ms: list(t)) =>
  ms
  |> List.map(nib_shapes)
  |> List.split
  |> TupleUtil.map2(ListUtil.single_elem);
