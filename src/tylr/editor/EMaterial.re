module Molded = {
  module Grout = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Tip.s;
  };
  module Tile = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | Unmolded(Tip.s)
      | Molded(Mold.t);
  };
  type t = Material.t(Grout.t, Tile.t);
};
include Molded;

module Labeled = {
  type t = Material.t(Tip.s, list(Label.t));
};

module Sorted = {
  type t = Material.t(unit, option(Sort.t));

  let consistent = (e: t, g: GMaterial.Sorted.t) =>
    switch (e, g) {
    | (_, None)
    | (Space | Grout () | Tile(Unmolded ()), Some(_)) => true
    | (Tile(Molded(s_e)), Some(s_g)) => Sort.eq(s_e, s_g)
    };
};
