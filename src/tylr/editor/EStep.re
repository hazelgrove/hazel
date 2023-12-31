[@deriving (show({with_path: false}), sexp, yojson, ord, hash)]
type t =
  | Mold(EMold.t)
  | Meld(ESort.t);

let of_g = (step: GWalk.Step.t): t =>
  switch (step) {
  | Mold(m) => Mold(Tile(m))
  | Meld(s) => Meld(Tile(s))
  };

module Set = {
  type nonrec t = list(t);
  module Syntax = ListUtil.Syntax;
  let entry_grout = (side: Dir.t, s: Sort.t) =>
    switch (side) {
    | L => [
        Mold(Grout((Convex, Convex))),
        Mold(Grout((Convex, Concave))),
        Meld(Grout(Sort.root)),
      ]
    | R => [
        Mold(Grout((Convex, Convex))),
        Mold(Grout((Concave, Convex))),
        Meld(Grout(Sort.root)),
      ]
    };
};
module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });
