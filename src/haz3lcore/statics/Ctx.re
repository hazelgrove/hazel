include CtxBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = ctx(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type co = co_ctx(Typ.mode);
