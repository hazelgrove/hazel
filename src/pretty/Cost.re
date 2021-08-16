open Sexplib.Std;
[@deriving (sexp, show)]
type t = {
  // Sum of the square of how much each line extends beyond max width.  Usually zero.
  overflow_cost: int,
  // Height minus one.  A.k.a. number of line breaks.
  height_cost: int,
};

let mk = (~overflow_cost: int, ~height_cost: int): t => {
  overflow_cost,
  height_cost,
};

let mk_height = (height_cost: int): t => mk(~overflow_cost=0, ~height_cost);

let mk_overflow = (overflow_cost: int): t =>
  mk(~overflow_cost, ~height_cost=0);

let zero: t = {overflow_cost: 0, height_cost: 0};

let inf: t = {overflow_cost: max_int, height_cost: max_int};

let add = (c1: t, c2: t): t => {
  overflow_cost: c1.overflow_cost + c2.overflow_cost,
  height_cost: c1.height_cost + c2.height_cost,
};

let eq = (c1: t, c2: t): bool =>
  c1.overflow_cost === c2.overflow_cost && c1.height_cost === c2.height_cost;

let lt = (c1: t, c2: t): bool =>
  if (c1.overflow_cost === c2.overflow_cost) {
    c1.height_cost < c2.height_cost;
  } else {
    c1.overflow_cost < c2.overflow_cost;
  };

let leq = (c1: t, c2: t): bool =>
  if (c1.overflow_cost === c2.overflow_cost) {
    c1.height_cost <= c2.height_cost;
  } else {
    c1.overflow_cost <= c2.overflow_cost;
  };
