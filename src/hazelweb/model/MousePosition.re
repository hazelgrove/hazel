open Sexplib.Std;

/**
 * The coordinates of the mouse (in px)
 * relative to the left and top edges of the document
 */
[@deriving sexp]
type t = {
  x: int,
  y: int,
};
