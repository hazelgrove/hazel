[@deriving (show({with_path: false}), sexp, yojson)]
type t('l, 'r) =
  | L('l)
  | R('r);

let l = x => L(x);
let r = x => R(x);

let is_L =
  fun
  | L(_) => true
  | R(_) => false;
let is_R = e => !is_L(e);

let get_L =
  fun
  | L(l) => Some(l)
  | R(_) => None;
let get_R =
  fun
  | R(r) => Some(r)
  | L(_) => None;
