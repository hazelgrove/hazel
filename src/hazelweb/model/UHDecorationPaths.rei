/**
 * A collection of paths to decorations in a given UH-term
 */
[@deriving sexp]
type t = {
  err_holes: list(CursorPath_common.steps),
  var_err_holes: list(CursorPath_common.steps),
  var_uses: list(CursorPath_common.steps),
  current_term: option(CursorPath_common.t),
};

let is_empty: t => bool;

/**
 * Given `dpaths` containing the decoration paths for
 * some UH-term, `take_step(n, dpaths)` returns the
 * decoration paths within the UH-term's `n`th child.
 */
let take_step: (int, t) => t;

/**
 * `current(shape, dpaths)` returns the shapes of decorations
 * adorning the root of the current UH-term
 */
let current: (TermShape.t, t) => list(UHDecorationShape.t);
