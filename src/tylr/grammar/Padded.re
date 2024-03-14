[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) = (Padding.t, 'a);

let mk = (~l=false, ~r=false, ~indent=true, a) => (
  Padding.mk(~l, ~r, ~indent, ()),
  a,
);
