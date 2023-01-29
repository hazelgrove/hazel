// todo: add must_match flag
type t =
  // Flat?
  | Convex
  | Concave(Sort.o, Prec.t);

let fits = (l, r) =>
  switch (l, r) {
  | (Convex, Concave(_))
  | (Concave(_), Convex) => true
  | (Convex, Convex)
  | (Concave(_), Concave(_)) => false
  };

let same_shape = (l, r) => !fits(l, r);

let root = Concave(Sort.root_o, Prec.min);
