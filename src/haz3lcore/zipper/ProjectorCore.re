open Util;

/* Projectors currently have two options for placeholder
 * shapes: A inline display of a given length, or a block
 * display with given length & height. Both of these can
 * depend on the projector model and info package */
[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Inline(int)
  | Block(Point.t);
