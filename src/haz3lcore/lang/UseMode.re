[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Nat
  | Int;

let default = Int;
