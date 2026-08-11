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
