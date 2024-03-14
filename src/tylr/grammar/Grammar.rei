[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  Sort.Map.t(Prec.Table.t(Regex.t(Sym.t(Label.t, Padded.t(Sort.t)))));
let v: t;
