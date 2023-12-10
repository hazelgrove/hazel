module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('g, 't) =
    | Space
    | Grout('g)
    | Tile('t);
};

module Tile = {
  // todo: flatten this with Space/Grout/Tile
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('u, 'm) =
    | Unmolded('u)
    | Molded('m);
};

module Labeled = {
  type t = Base.t(Tip.s, list(Label.t));
};

module Sorted = {
  type t = Base.t(unit, Sort.t);
};

module Molded = {
  type t = Base.t(Tip.s, Tile.t(Tip.s, GMold.t));
};
include Molded;
