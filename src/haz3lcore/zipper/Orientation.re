open Util;

module type S = {
  let d: Direction.t;
  let orient: (('a, 'a)) => ('a, 'a);
};

module L: S = {
  let d = Direction.Left;
  let orient = ((l, r)) => (l, r);
};
module R: S = {
  let d = Direction.Right;
  let orient = ((l, r)) => (r, l);
};
