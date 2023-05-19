[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  livelit: Livelit.t,
};
let id = t => t.id;
