[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | Empty
  | Full('a);
