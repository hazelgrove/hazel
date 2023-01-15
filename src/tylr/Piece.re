type shape =
  | T(Tile.t)
  | G(Grout.t);

type t = {
  shape,
  space: (Space.t, Space.t),
};

let mk = (~l=Space.empty, ~r=Space.empty, shape) => {shape, space: (l, r)};

let length = (~with_space as _=false, p: t) => {
  switch (p.shape) {
  | T(t) => Tile.length(t)
  | G(g) => Grout.length(g)
  };
};
