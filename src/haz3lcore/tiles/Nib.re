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

  let fitting =
    fun
    | Convex => concave()
    | Concave(_) => Convex;

  let flip =
    fun
    | Convex => concave()
    | Concave(_) => Convex;

  let absolute = (d: Util.Direction.t, s: t): Util.Direction.t =>
    /* The direction an s-shaped nib on the d-hand side is facing */
    switch (s) {
    | Convex => d
    | Concave(_) => Util.Direction.toggle(d)
    };

  let relative = (nib: Util.Direction.t, side: Util.Direction.t): t =>
    nib == side ? Convex : concave();
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  shape: Shape.t,
  sort: Sort.t,
};

let shape = n => n.shape;

let fits = (l: t, r: t): bool =>
  l.sort == r.sort && Shape.fits(l.shape, r.shape);

let fitting = (nib: t): t => {...nib, shape: Shape.fitting(nib.shape)};

let flip = (nib: t) => {...nib, shape: Shape.flip(nib.shape)};

// let toggle = (nib: t) => {
//   ...nib,
//   orientation: Direction.toggle(nib.orientation),
// };

// let sort_consistent = (nib: t, nib': t) => nib.sort == nib'.sort;

// let of_sort = sort => [
//   {sort, orientation: Left},
//   {sort, orientation: Right},
// ];
