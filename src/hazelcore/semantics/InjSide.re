[@deriving sexp]
type t =
  | L
  | R;

let pick = (side, l, r) =>
  switch (side) {
  | L => l
  | R => r
  };
