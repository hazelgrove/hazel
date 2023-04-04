open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Proto.t);
