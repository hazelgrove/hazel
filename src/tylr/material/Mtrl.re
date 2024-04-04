module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('t) =
    | Space
    | Grout
    | Tile('t);
};
include Base;

let tile = t => Tile(t);

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
  let padding =
    fun
    | Space => Padding.none
    | Grout => Padding.mk()
    | Tile(lbl) => Label.padding(lbl);
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
module NT = Sorted;
module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
};

module Regex = {
  include Regex;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Regex.t(Sym.t);
};
// module RFrame = {
//   include RFrame;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = RFrame(Sym.t);
// };
