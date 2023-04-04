// todo: add must_match flag
type t =
  // Flat?
  | Convex
  | Concave(Sort.o, Prec.t);

let is_concave =
  fun
  | Convex => None
  | Concave(s, p) => Some((s, p));

let consistent = (l, r) =>
  switch (l, r) {
  | (Convex, Convex) => true
  | (Concave(l, _), Concave(r, _)) => Sort.consistent(l, r)
  | _ => false
  };

let fits = (l, r) =>
  switch (l, r) {
  | (Convex, Concave(_))
  | (Concave(_), Convex) => true
  | (Convex, Convex)
  | (Concave(_), Concave(_)) => false
  };

let same_shape = (l, r) => !fits(l, r);

let root = Concave(Sort.root_o, Prec.min);

let lt = (~assoc, l: t, r: t): option((Sort.o, Prec.t)) =>
  switch (l) {
  | Convex => None
  | Concave(s, p) =>
    switch (r) {
    | Convex => Some((s, p))
    | Concave(t, q) =>
      // todo: review this sort comparison
      // esp in light of losing sort order
      Sort.compare_o(s, t) <= 0
      && (p < q || p == q && assoc(p) == Some(Dir.L))
        ? Some((s, p)) : None
    }
  };

let gt = (~assoc, l: t, r: t): option((Sort.o, Prec.t)) =>
  switch (r) {
  | Convex => None
  | Concave(s, p) =>
    switch (l) {
    | Convex => Some((s, p))
    | Concave(t, q) =>
      Sort.compare_o(t, s) >= 0
      && (p < q || p == q && assoc(p) == Some(Dir.R))
        ? Some((s, p)) : None
    }
  };
