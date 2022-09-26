[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
let eq: (t, t) => bool;
