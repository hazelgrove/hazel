// todo: add must_match flag
type t =
  // Flat?
  | Convex
  | Concave(Sort.o, Prec.t);
