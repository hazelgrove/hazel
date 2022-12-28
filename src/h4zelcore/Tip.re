type t =
  // Flat?
  | Convex
  | Concave(Sort.t);

// let compare = (t1, t2) =>
//   switch (t1, t2) {
//   | (Convex, Convex) => 0
//   | (Convex, Concave(_)) => (-1)
//   | (Concave(_), Convex) => 1
//   | (Concave(s1, p1), Concave(s2, p2)) =>
//     let c = Sort.compare(s1, s2);
//     c == 0 ? Prec.compare(p1, p2) : c;
//   };
