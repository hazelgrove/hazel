[@deriving (show({with_path: false}), sexp, yojson)]
type t = PBNF.t(Sym.t(Label.t, Sort.t));
let v: t;
