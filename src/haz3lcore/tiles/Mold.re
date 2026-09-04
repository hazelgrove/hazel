open Util_web;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  out: Sort.t,
  in_: list(Sort.t),
  nibs: Nibs.t,
};

let mk_op = (out, in_) => {
  let n =
    Nib.{
      shape: Convex,
      sort: out,
    };
  {
    out,
    in_,
    nibs: (n, n),
  };
};
let mk_pre = (p, out, in_) => {
  let l =
    Nib.{
      shape: Convex,
      sort: out,
    };
  let r =
    Nib.{
      shape: Concave(p),
      sort: out,
    };
  {
    out,
    in_,
    nibs: (l, r),
  };
};
// Prefix form where the body (right nib) has a different sort than out
let mk_pre' = (p, out, in_, sort_r) => {
  let l =
    Nib.{
      shape: Convex,
      sort: out,
    };
  let r =
    Nib.{
      shape: Concave(p),
      sort: sort_r,
    };
  {
    out,
    in_,
    nibs: (l, r),
  };
};
let mk_post = (p, out, in_) => {
  let l =
    Nib.{
      shape: Concave(p),
      sort: out,
    };
  let r =
    Nib.{
      shape: Convex,
      sort: out,
    };
  {
    out,
    in_,
    nibs: (l, r),
  };
};
let mk_bin = (~l=?, ~r=?, p, out, in_) => {
  let l = Option.value(l, ~default=out);
  let r = Option.value(r, ~default=out);
  let nib = sort =>
    Nib.{
      sort,
      shape: Concave(p),
    };
  {
    out,
    in_,
    nibs: (nib(l), nib(r)),
  };
};

// forms where tips can be different than out sort

let mk_bin' = (p, out, sort_l, in_, sort_r) => {
  let l =
    Nib.{
      shape: Concave(p),
      sort: sort_l,
    };
  let r =
    Nib.{
      shape: Concave(p),
      sort: sort_r,
    };
  {
    out,
    in_,
    nibs: (l, r),
  };
};

let nibs = (~index, mold: t): Nibs.t => {
  let (l, r) = mold.nibs;
  let in_ = mold.in_;
  /* Inner-shard nibs pull their sort from mold.in_. If remolding has
     assigned a tile a mold whose arity doesn't match its shards (seen in
     practice for some in-progress derivation tiles, e.g. `(` `,` `)`
     before the segment is fully reassembled), `List.nth` would throw.
     We fall back to `Any` so the tile still renders while upstream
     remolding stabilizes. */
  let l =
    index == 0
      ? l
      : Nib.{
          shape: Shape.concave(),
          sort:
            try(List.nth(in_, index - 1)) {
            | Failure(_) => Sort.Any
            },
        };
  let r =
    index == List.length(in_)
      ? r
      : Nib.{
          shape: Shape.concave(),
          sort:
            try(List.nth(in_, index)) {
            | Failure(_) => Sort.Any
            },
        };
  (l, r);
};

let nib_shapes = (~index, mold: t): Nibs.shapes => {
  let (nib_l, nib_r) = nibs(~index, mold);
  (nib_l.shape, nib_r.shape);
};

let is_infix_op = (mold: t): bool =>
  switch (mold.nibs, mold.in_) {
  | (({shape: Concave(_), _}, {shape: Concave(_), _}), []) => true
  | _ => false
  };

let is_prefix_op = (mold: t): bool =>
  switch (mold.nibs, mold.in_) {
  | (({shape: Convex, _}, {shape: Concave(_), _}), []) => true
  | _ => false
  };
