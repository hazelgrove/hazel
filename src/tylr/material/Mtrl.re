module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('t) =
    | Space
    | Grout
    | Tile('t);
};
include Base;

let is_space =
  fun
  | Space => true
  | _ => false;

module Label = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Label.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
module Sort = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Sort.t);
  let root = Tile(Sort.root);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
module PSort = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Padded.t(Sort.t));
  let root = Tile((Padding.root, Sort.root));
  let indent =
    fun
    | Space
    | Tile((false, _)) => false
    | Grout
    | Tile((true, _)) => true;
};

module Sym = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(Label.t, PSort.t);
};
