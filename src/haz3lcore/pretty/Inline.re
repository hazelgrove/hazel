[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Single
  | Zzt
  | Compound;
