[@deriving sexp]
type tile_step = int;
[@deriving sexp]
type caret_step = int;

[@deriving sexp]
type two_step = (tile_step, ChildStep.t);
[@deriving sexp]
type steps = list(two_step);

[@deriving sexp]
type t = (steps, caret_step);

[@deriving sexp]
type cursor =
  | Pointing(t)
  | Selecting(t, t)
  | Restructuring(t, t, t);

let to_zipper: (cursor, Term_exp.t) => Zipper.t;
let of_zipper: Zipper.t => (cursor, Term_exp.t);