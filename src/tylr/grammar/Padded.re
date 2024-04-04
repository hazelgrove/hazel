[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) = (Padding.t, 'a);

let mk = (~h_l=true, ~h_r=true, ~v_l=true, ~v_r=true, ~indent=true, a) => (
  Padding.mk(~h_l, ~h_r, ~v_l, ~v_r, ~indent, ()),
  a,
);

let get = snd;

let map = (f, (p, a): t(_)) => (p, f(a));
