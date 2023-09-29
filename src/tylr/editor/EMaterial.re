module Tile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('u, 'm) =
    | Unmolded('u)
    | Molded('m);
};
type t = Material.t(Tip.s, Tile.t(Tip.s, Mold.t));

module Sorted = {
  type t = Material.t(unit, Tile.t(unit, Sort.t));

  let consistent = (e: t, g: GMaterial.Sorted.t) =>
    switch (e, g) {
    | (_, None)
    | (Space | Grout () | Tile(Unmolded ()), Some(_)) => true
    | (Tile(Molded(s_e)), Some(s_g)) => Sort.eq(s_e, s_g)
    };
};
