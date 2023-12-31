module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('g, 't) =
    | Space
    | Grout('g)
    | Tile('t);
};

module Melded = {
  // the synthetic (a la bidir types) sort and shape of a cell's contents
  // Space indicates no contents of structure
  type t = Base.t((Sort.t, Tip.s), (Sort.t, Prec.t, Tip.s));
};
