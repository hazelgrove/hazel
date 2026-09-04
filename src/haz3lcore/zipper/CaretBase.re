open Util_web;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Outer
  | Inner(int);
