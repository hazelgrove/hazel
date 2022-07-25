[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Nib.t, Nib.t);

type shapes = (Nib.Shape.t, Nib.Shape.t);

let flip = ((l, r): t) => (r, l);

let of_hole = sort => Nib.({sort, shape: Convex}, {sort, shape: Convex});

let fitting = ((l, r): t) => (Nib.fitting(l), Nib.fitting(r));
