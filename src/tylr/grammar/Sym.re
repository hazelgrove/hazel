[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('t, 'nt) =
  | T('t)
  | NT('nt);

let get_t =
  fun
  | T(t) => Some(t)
  | NT(_) => None;
let is_t = sym => Option.is_some(get_t(sym));
