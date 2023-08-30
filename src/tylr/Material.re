open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('g, 't) =
  | Grout('g)
  | Tile('t);

let map_g = f =>
  fun
  | Grout(g) => Grout(f(g))
  | Tile(_) as m => m;
let map_t = f =>
  fun
  | Grout(_) as m => m
  | Tile(a) => Tile(f(a));

let to_option =
  fun
  | Grout () => None
  | Tile(t) => Some(t);

// for use in submodules below
[@deriving (show({with_path: false}), sexp, yojson)]
type m('g, 't) = t('g, 't);

module Molded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = m(Tip.s, Mold.t);
};

module Labeled = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = m(unit, Label.t);
  let of_molded = m => m |> map_g(_ => ()) |> map_t(Mold.label);
};

module Sorted = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = m(unit, Sort.t);
  let of_molded = m => m |> map_g(_ => ()) |> map_t(Mold.sort_);
};
