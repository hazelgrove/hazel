[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | L
  | R;

let toggle =
  fun
  | L => R
  | R => L;

let order: 'a. (t, ('a, 'a)) => ('a, 'a) =
  (d, (x, y)) =>
    switch (d) {
    | L => (x, y)
    | R => (y, x)
    };

let pick = (d, p) => fst(order(d, p));
