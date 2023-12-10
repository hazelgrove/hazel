module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('g, 't) =
    | Space
    | Grout('g)
    | Tile('t);
};

module Labeled = {
  type t = Base.t(Tip.s, list(Label.t));
};

module Sorted = {
  // used to signify expected sort
  // Space indicates no term expected
  type t = Base.t(Sort.t, GSort.t);
};

module Molded = {
  type t = Base.t(Tip.s, GMold.t);
};
include Molded;

module Melded = {k
  // the synthetic (a la bidir types) sort and shape of a cell's contents
  // Space indicates no contents of structure
  type t = Base.t((Sort.t, Tip.s), (Sort.t, Prec.t, Tip.s));
};