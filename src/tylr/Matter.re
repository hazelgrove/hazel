type t('a) =
  | Grout
  | Tile('a);

type sorted = t(Sort.t);
type s = sorted; // abbrev

type molded = t(Mold.t);
type m = molded; // abbrev

let map = f =>
  fun
  | Grout => Grout
  | Tile(a) => Tile(f(a));
