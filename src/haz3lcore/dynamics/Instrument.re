[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Eval
  | Pause;

let equal = (t1, t2) => t1 == t2;
