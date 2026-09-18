[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Left
  | Right;
let toggle: t => t;
