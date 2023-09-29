open Material;

include Terrace;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Terrace.t(Molded.t, Sorted.t);

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Set = {
  include Set.Make(Ord);

  let pick = ();
};
