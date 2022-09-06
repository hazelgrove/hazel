open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Convex
  | Concave;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  shape,
};

let id = g => g.id;

let shapes = g =>
  switch (g.shape) {
  | Convex => Nib.Shape.(Convex, Convex)
  | Concave => Nib.Shape.(Concave(Precedence.min), Concave(Precedence.min))
  };

// assumes same shape on both sides
let mk_fits_shape = (s: Nib.Shape.t): IdGen.t(t) => {
  open IdGen.Syntax;
  let+ id = IdGen.fresh;
  let shape =
    switch (s) {
    | Convex => Concave
    | Concave(_) => Convex
    };
  {id, shape};
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
  open IdGen.Syntax;
  let* (l_hole, l_shape) =
    switch (l) {
    | {shape: Concave(_), sort} when sort != s && sort != r.sort =>
      let+ id = IdGen.fresh;
      ([{id, shape: Convex}], Nib.Shape.Convex);
    | _ => return(([], l.shape))
    };
  let* (r_shape, r_hole) =
    switch (r) {
    | {shape: Concave(_), sort} when sort != s && sort != l.sort =>
      let+ id = IdGen.fresh;
      (Nib.Shape.Convex, [{id, shape: Convex}]);
    | _ => return((r.shape, []))
    };
  let+ mid_hole =
    Nib.Shape.fits(l_shape, r_shape)
      ? return([])
      : {
        let+ g = mk_fits_shape(l_shape);
        [g];
      };
  List.concat([l_hole, mid_hole, r_hole]);
};
