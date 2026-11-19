[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Exp
  | Val;
