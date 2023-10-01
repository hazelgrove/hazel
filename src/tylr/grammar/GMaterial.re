[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Material.t(Tip.s, Mold.t);

module Sorted = {
  type t = option(Sort.t);
};

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
