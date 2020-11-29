module HoleMap =
  Map.Make({
    type t = CursorPath.steps;
    let compare = CursorPath.compare_steps;
  });

type filled_holes =
  // need constructor to prevent type synonym cycle
  | F(HoleMap.t((UHExp.t, filled_holes)));

let filled_holes_of_sexp = _ =>
  failwith("Synthesizing.filled_holes_of_sexp todo");
let sexp_of_filled_holes = _ =>
  failwith("Synthesizing.sexp_of_filled_holes todo");

/**
 * Top-down zipper representing synthesis navigation
 */
type t = (CursorPath.steps, z)
and z =
  | Filling(ZList.t(UHExp.t, UHExp.t))
  | Filled(t, filled_holes);

let t_of_sexp = _ => failwith("Synthesizing.t_of_sexp todo");
let sexp_of_t = _ => failwith("Synthesizing.sexp_of_t todo");
