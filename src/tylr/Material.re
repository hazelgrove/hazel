[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | Grout(Tip.t, Tip.t)
  | Tile('a);

[@deriving (show({with_path: false}), sexp, yojson)]
type labeled = t(Label.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type sorted = t(Sort.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type molded = t(Mold.t);

let map = f =>
  fun
  | Grout(_) as m => m
  | Tile(a) => Tile(f(a));
