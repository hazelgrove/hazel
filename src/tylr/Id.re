open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
let compare = Int.compare;
let invalid = (-1);

module Map = Util.IntMap;

module Gen = {
  let t: ref(t) = ref(0);

  let next = (): t => {
    let id = t^;
    t := id + 1;
    id;
  };
};
