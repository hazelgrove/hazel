module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('t) =
    | Space
    | Grout
    | Tile('t);
};
include Base;

// module Labeled = {
//   type t = Base.t(Label.t);
// };
