[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('t) =
  | Tok('t)
  | Kid(Sort.t);

let get_tok =
  fun
  | Kid(_) => None
  | Tok(t) => Some(t);
let is_tok = a => Option.is_some(get_tok(a));

let is_kid =
  fun
  | Tok(_) => None
  | Kid(s) => Some(s);
