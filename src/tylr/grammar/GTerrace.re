open Material;

include Terrace;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Terrace.t(GMaterial.t, Sorted.t);

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Set = Set.Make(Ord);
