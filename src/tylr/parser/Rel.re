[@deriving (show({with_path: false}), sexp, yojson)]
type t('eq, 'neq) =
  | Eq('eq)
  | Neq('neq);
