[@deriving (show({with_path: false}), sexp, yojson)]
type t = Sort.Map.t(Prec.Table.t(Regex.t(Label.t)));
let v: t;
