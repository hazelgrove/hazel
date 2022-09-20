open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Convex
  | Concave;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  sort: Sort.t,
  shape,
};

let id = g => g.id;

let shapes = g =>
  switch (g.shape) {
  | Convex => Nib.Shape.(Convex, Convex)
  | Concave => Nib.Shape.(concave(), concave())
  };

let mold = g => {
  let (l, r) = shapes(g);
  Mold.{
    out: g.sort,
    in_: [],
    nibs: Nib.({shape: l, sort: Any}, {shape: r, sort: Any}),
  };
};

// assumes same shape on both sides
let mk_fits_shape = (s: Nib.Shape.t, sort): IdGen.t(t) => {
  open IdGen.Syntax;
  let+ id = IdGen.fresh;
  let shape =
    switch (s) {
    | Convex => Concave
    | Concave(_) => Convex
    };
  {id, sort, shape};
};
//let mk_fits = ((l, r): Nibs.shapes): option(IdGen.t(t)) =>
//  Nib.Shape.fits(l, r) ? None : Some(mk_fits_shape(l));

let fits_shape = (g: t, s: Nib.Shape.t) =>
  switch (g.shape, s) {
  | (Convex, Convex)
  | (Concave, Concave(_)) => false
  | (Convex, Concave(_))
  | (Concave, Convex) => true
  };

let fits = (g: t, g': t) =>
  switch (g.shape, g'.shape) {
  | (Convex, Convex)
  | (Concave, Concave) => false
  | (Convex, Concave)
  | (Concave, Convex) => true
  };

let merge = (gs: list(t)): option(t) =>
  switch (gs) {
  | [] => None
  | [hd, ...tl] =>
    switch (ListUtil.split_last_opt(tl)) {
    | None => Some(hd)
    | Some((_, ft)) => hd.shape == ft.shape ? Some(hd) : None
    }
  };

let mk = ((l, r): Nibs.t, s: Sort.t): IdGen.t(list(t)) => {
  open // TODO clean up
       IdGen.Syntax;
  let* (l_hole, l_nib) =
    switch (l) {
    | {shape: Concave(_), sort: Rul} when s == Exp => return(([], l))
    | {shape: Concave(_), sort} when sort != s && sort != r.sort =>
      let+ id = IdGen.fresh;
      ([{id, sort, shape: Convex}], Nib.{sort, shape: Convex});
    | _ => return(([], l))
    };
  let* (r_nib, r_hole) =
    switch (r) {
    | {shape: Concave(_), sort: Rul} when s == Exp => return((r, []))
    | {shape: Concave(_), sort} when sort != s && sort != l.sort =>
      let+ id = IdGen.fresh;
      (Nib.{sort, shape: Convex}, [{id, sort, shape: Convex}]);
    | _ => return((r, []))
    };
  let+ mid_hole =
    Nib.Shape.fits(l_nib.shape, r_nib.shape)
      ? return([])
      : {
        let s = Sort.eq(l_nib.sort, r_nib.sort) ? l_nib.sort : s;
        let+ g = mk_fits_shape(l_nib.shape, s);
        [g];
      };
  List.concat([l_hole, mid_hole, r_hole]);
};
