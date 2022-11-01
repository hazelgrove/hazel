[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Type(Typ.t);
