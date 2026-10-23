/* A value of one of two types. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t('l, 'r) =
  | L('l)
  | R('r);
