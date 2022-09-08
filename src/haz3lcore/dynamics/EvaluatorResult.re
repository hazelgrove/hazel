[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

let unbox =
  fun
  | BoxedValue(d)
  | Indet(d) => d;

let fast_equal = (r1, r2) =>
  switch (r1, r2) {
  | (BoxedValue(d1), BoxedValue(d2))
  | (Indet(d1), Indet(d2)) => DHExp.fast_equal(d1, d2)
  | _ => false
  };
