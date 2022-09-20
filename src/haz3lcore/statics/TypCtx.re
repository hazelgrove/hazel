include CtxBase;

module Kind = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = ctx(Kind.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type co = co_ctx(Kind.mode);
