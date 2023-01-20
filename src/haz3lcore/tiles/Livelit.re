[@deriving (show({with_path: false}), sexp, yojson)]
type t = {id: Id.t};

let id = w => w.id;
