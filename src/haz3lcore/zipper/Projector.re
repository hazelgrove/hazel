[@deriving (show({with_path: false}), sexp, yojson)]
type p =
  | Normal
  | Fold;

let toggle_fold: p => p =
  fun
  | Normal => Fold
  | Fold => Normal;

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(p);
  let empty = empty;
  let add = add;
  let find = find_opt;
};

type t = p;
