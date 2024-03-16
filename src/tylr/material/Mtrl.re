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
  | Grout
  | Tile(_) => false;
let is_grout =
  fun
  | Grout => true
  | Space
  | Tile(_) => false;

module Labeled = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Label.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module Sorted = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Sort.t);
  let root = Tile(Sort.root);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module T = Labeled;
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Padded.t(Sorted.t);
  let grout = Padded.mk(Grout);
};
module Sym = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
};
