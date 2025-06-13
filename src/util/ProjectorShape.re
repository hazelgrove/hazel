include Sexplib.Std;
include Ppx_yojson_conv_lib.Yojson_conv.Primitives;

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

let token = (shape: t): string =>
  switch (shape.vertical) {
  | Inline
  | Tab(_) => String.make(shape.horizontal, ' ')
  | Block(num_lb) =>
    String.make(num_lb, '\n') ++ String.make(shape.horizontal, ' ')
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type shape = t;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(shape);

  let empty: t = Id.Map.empty;

  let lookup = (id: Id.t, shape_map: t): shape =>
    switch (Id.Map.find_opt(id, shape_map)) {
    | None => inline(0) //TODO: error reporting
    | Some(shape) => shape
    };
};
