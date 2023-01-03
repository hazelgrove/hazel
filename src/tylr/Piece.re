type shape =
  | T(Tile.t)
  | G(Grout.t);

type t = {
  // id: Id.t,
  shape,
  space: (Space.t, Space.t),
};

module Z = {
  type t =
    | Shape(int)
    | Space(Dir.t, int);
};
