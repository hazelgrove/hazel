[@deriving (show({with_path: false}), sexp, yojson)]
type t = Sort.Map.t(Precex.t(Sort.t));
let v: t;
