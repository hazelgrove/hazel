[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Pause
  | Eval;
