[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: TermCtx.t,
  typ: TypCtx.t,
};
