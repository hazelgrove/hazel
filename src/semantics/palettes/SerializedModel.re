open Sexplib.Std;

[@deriving (show({with_path: false}), sexp)]
type t = string;
