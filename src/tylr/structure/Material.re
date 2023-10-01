open Sexplib.Std;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('g, 't) =
    | Space
    | Grout('g)
    | Tile('t);
};

module Tile = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('u, 'm) =
    | Unmolded('u)
    | Molded('m);
};

// module Sorted = {
//   type t = Base.t(unit, Sort.t);
// };

module Molded = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Tip.s, Tile.t(Tip.s, Mold.t));
};
include Molded;

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
  type t = Base.t(unit, Tile.t(unit, Label.t));
  // let of_molded = m => m |> map_g(_ => ()) |> map_t(Mold.label);
};

module Sorted = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = m(unit, Sort.t);
  let of_molded = m => m |> map_g(_ => ()) |> map_t(Mold.sort_);
};
