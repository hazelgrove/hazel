[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | Grout
  | Tile('a);

[@deriving (show({with_path: false}), sexp, yojson)]
type labeled = t(Label.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type sorted = t(Sort.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type molded = t(Mold.t);

let map = f =>
  fun
  | Grout => Grout
  | Tile(a) => Tile(f(a));
