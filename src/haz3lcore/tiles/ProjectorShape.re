open Util;

/* A projector shape determines the space left for
 * that projector, and how text flows around a projector
 * in a text editor. All projectors have a horizontal
 * extend (in characters), and the vertical extent may
 * be either 1 character (Inline), or it may insert
 * an additional number of linebreaks */
[@deriving (show({with_path: false}), sexp, yojson)]
type vertical =
  | Inline
  | Block(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  horizontal: int,
  vertical,
};

let num_lb = (shape: t): int =>
  switch (shape.vertical) {
  | Inline => 0
  | Block(num_lbs) => num_lbs
  };

let inline = (width: int): t => {horizontal: width, vertical: Inline};
let default: t = inline(0);
