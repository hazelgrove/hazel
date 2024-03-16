[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) = (Padding.t, 'a);

let mk = (~space=true, ~indent=true, a) => (
  Padding.mk(~space, ~indent, ()),
  a,
);
