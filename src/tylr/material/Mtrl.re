module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('t) =
    | Space
    | Grout
    | Tile('t);
};
include Base;

module Label = {
  type t = Base.t(Label.t);
};
module Sort = {
  type t = Base.t(Sort.t);
};

module Sym = {
  type t = Sym.t(Label.t, Sort.t);
};
