open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('g, 't) =
  | Grout('g)
  | Tile('t);

[@deriving (show({with_path: false}), sexp, yojson)]
type labeled = t(unit, Label.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type sorted = t(unit, Sort.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type molded = t(Tip.s, Mold.t);

let map_g = f =>
  fun
  | Grout(g) => Grout(f(g))
  | Tile(_) as m => m;
let map_t = f =>
  fun
  | Grout(_) as m => m
  | Tile(a) => Tile(f(a));

let labeled_of_molded = m => m |> map_g(_ => ()) |> map_t(Mold.label);
let sorted_of_molded = m => m |> map_g(_ => ()) |> map_t(Mold.sort_);

let to_option =
  fun
  | Grout () => None
  | Tile(t) => Some(t);
