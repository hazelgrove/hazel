include CtxBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = ctx(Typ.t);
