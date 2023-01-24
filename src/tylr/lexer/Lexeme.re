type t =
  | S(Space.t) // todo: Space.elem
  | G(Grout.t)
  | T(Tile.t);

let is_porous =
  fun
  | S(_)
  | G(_) => true
  | T(_) => false;
