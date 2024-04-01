[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('t, 'nt) =
  | T('t)
  | NT('nt);

let t = t => T(t);
let nt = nt => NT(nt);

let get_t =
  fun
  | T(t) => Some(t)
  | NT(_) => None;
let is_t = sym => Option.is_some(get_t(sym));

let get_nt =
  fun
  | NT(nt) => Some(nt)
  | T(_) => None;

let map = (f_t, f_nt) =>
  fun
  | T(t) => T(f_t(t))
  | NT(nt) => NT(f_nt(nt));
