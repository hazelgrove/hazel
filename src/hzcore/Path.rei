[@deriving sexp]
type two_step = (TileStep.t, ChildStep.t);
[@deriving sexp]
type steps = list(two_step);

// TODO add examples
// TODO write about the flexibility of indexing
// eg can have a caret step directly into a subtree
[@deriving sexp]
type t = (steps, CaretStep.t);

let is_valid_path_to_token: (steps, Term_exp.t) => bool;
let is_valid_path_to_child: (steps, Term_exp.t) => bool;

// add comment about failure if is_valid false
let to_zipper: (cursor, Term_exp.t) => Zipper.t;
let of_zipper: Zipper.t => (cursor, Term_exp.t);
