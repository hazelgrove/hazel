[@deriving (show({with_path: false}), sexp, yojson)]
type t = Sort.Map.t(Prec.Table.t(Regex.t(Sym.t(Label.t, Sort.t))));
// PBNF.t(Sym.t(Label.t, Sort.t));
let v: t;
