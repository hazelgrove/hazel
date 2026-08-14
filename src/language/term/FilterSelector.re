[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Exp
  | Val;
