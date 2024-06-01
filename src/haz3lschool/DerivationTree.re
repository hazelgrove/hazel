open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | Node('a, list(t('a)));
