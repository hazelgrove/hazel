[@deriving (show({with_path: false}), sexp, yojson)]
type t('eq, 'neq) =
  | Eq('eq)
  | Neq('neq);

let is_eq =
  fun
  | Eq(eq) => Some(eq)
  | Neq(_) => None;
