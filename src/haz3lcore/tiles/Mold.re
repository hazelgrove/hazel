open Sexplib.Std;
open Util;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

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
let mk_bin = (~l=?, ~r=?, p, out, in_) => {
  let l = Option.value(l, ~default=out);
  let r = Option.value(r, ~default=out);
  let nib = sort => Nib.{sort, shape: Concave(p)};
  {out, in_, nibs: (nib(l), nib(r))};
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

let of_secondary = (l: Nib.t) => {
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

let is_infix_op = (mold: t): bool =>
  switch (mold.nibs, mold.in_) {
  | (({shape: Concave(_), _}, {shape: Concave(_), _}), []) => true
  | _ => false
  };

let chevron = (sort: Sort.t, p: Precedence.t, d: Util.Direction.t): t =>
  switch (d) {
  | Right => mk_post(p, sort, [])
  | Left => mk_pre(p, sort, [])
  };
