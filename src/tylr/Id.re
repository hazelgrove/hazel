open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
let compare = Int.compare;
let invalid = (-1);

module Map = Util.IntMap;

module Gen = {
  let t: ref(Id.t) = ref(0);

  let next = (): Id.t => {
    let id = t^;
    t := id + 1;
    id;
  };
};
