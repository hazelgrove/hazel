[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | L
  | R;

let toggle =
  fun
  | L => R
  | R => L;

let order = (d, (x, y)) =>
  switch (d) {
  | L => (x, y)
  | R => (y, x)
  };
let unorder = order;

let choose = (d, p) => fst(order(d, p));
