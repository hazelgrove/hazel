[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Parent.t, Siblings.t);
