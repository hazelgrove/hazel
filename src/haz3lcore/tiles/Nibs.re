[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Nib.t, Nib.t);

[@deriving show]
type shapes = (Nib.Shape.t, Nib.Shape.t);

let flip = ((l, r): t) => (r, l);
