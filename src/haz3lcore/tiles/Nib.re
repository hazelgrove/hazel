open Util;

module Shape = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Convex
    | Concave(Precedence.t);

  let concave = (~p=?, ()) => {
    let p =
      switch (p) {
      | None => Precedence.min
      | Some(p) => p
      };
    Concave(p);
  };

  let fits = (l: t, r: t) =>
    switch (l, r) {
    | (Convex, Concave(_))
    | (Concave(_), Convex) => true
    | (Convex, Convex)
    | (Concave(_), Concave(_)) => false
    };

  let flip =
    fun
    | Convex => concave()
    | Concave(_) => Convex;

  let absolute = (d: Direction.t, s: t): Direction.t =>
    /* The direction an s-shaped nib on the d-hand side is facing */
    switch (s) {
    | Convex => d
    | Concave(_) => Direction.toggle(d)
    };

  let relative = (nib: Direction.t, side: Direction.t): t =>
    nib == side ? Convex : concave();

  let direction_of = (d: Direction.t, shape: t): Direction.t =>
    switch (shape) {
    | Convex => d
    | Concave(_) => Direction.toggle(d)
    };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  shape: Shape.t,
  sort: Sort.t,
};

let shape = n => n.shape;

let flip = (nib: t) => {...nib, shape: Shape.flip(nib.shape)};
