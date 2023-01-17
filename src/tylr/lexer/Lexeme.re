type t =
  | S(Space.t)
  | G(Grout.t)
  | T(Tile.t);

let is_porous =
  fun
  | S(_)
  | G(_) => true
  | T(_) => false;
