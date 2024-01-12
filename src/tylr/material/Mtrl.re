module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('t) =
    | Space
    | Grout
    | Tile('t);
};
include Base;

module Label = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Label.t);
};
module Sort = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Sort.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module Sym = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(Label.t, Sort.t);
};
