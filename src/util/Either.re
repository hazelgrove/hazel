[@deriving (show({with_path: false}), sexp, yojson)]
type t('l, 'r) =
  | L('l)
  | R('r);

let l = x => L(x);
let r = x => R(x);

let is_l =
  fun
  | L(_) => true
  | R(_) => false;
let is_r = e => !is_l(e);

let get_l =
  fun
  | L(l) => Some(l)
  | R(_) => None;
let get_r =
  fun
  | R(r) => Some(r)
  | L(_) => None;

let map_r = f =>
  fun
  | L(_) as l => l
  | R(r) => R(f(r));
