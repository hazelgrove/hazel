/**
 * A collection of paths to decorations in a given UH-term
 */
[@deriving sexp]
type t = {
  err_holes: list(CursorPath.steps),
  var_err_holes: list(CursorPath.steps),
  var_uses: list(CursorPath.steps),
  tyvar_uses: list(CursorPath.steps),
  current_term: option(CursorPath.t),
};

let is_empty: t => bool;

/**
 * Given the decoration paths `dpaths` for some UH-term,
 * `take_step(n, dpaths)` returns the decoration paths
 * within the UH-term's `n`th child
 */
let take_step: (int, t) => t;

/**
 * Given the decoration paths `dpaths` for some UH-term of
 * shape `s`, `current(s, dpaths)` returns the shapes of
 * decorations adorning the UH-term root
 */
let current: (TermShape.t, t) => list(UHDecorationShape.t);
