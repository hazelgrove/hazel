[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | H(Dir.t)
  | V(Dir.t);
