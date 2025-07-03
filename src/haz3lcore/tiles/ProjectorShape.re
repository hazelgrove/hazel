open Util;

/* A projector shape determines the space left for
 * that projector, and how text flows around a projector
 * in a text editor. All projectors have a horizontal
 * extend (in characters), and the vertical extent may
 * be either 1 character (Inline), or it may insert
 * an additional number of linebreaks, either immediately
 * after the projector (Block style) or defer them to
 * the next linebreak (Tab style). In the latter case,
 * if there are multiple Tab projectors on a line, the
 * total extra linebreaks inserted is the maxium required
 * to accomodate them */
[@deriving (show({with_path: false}), sexp, yojson)]
type vertical =
  | Inline
  | Tab(int)
  | Block(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  horizontal: int,
  vertical,
};

let inline = (width: int): t => {
  horizontal: width,
  vertical: Inline,
};
let default: t = inline(0);
