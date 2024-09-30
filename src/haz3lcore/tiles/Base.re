open Util;

/* The different kinds of projector. New projectors
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Fold
  | Info
  | Checkbox
  | Slider
  | SliderF
  | TextArea;

[@deriving (show({with_path: false}), sexp, yojson)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector)
and tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: list(int),
  children: list(segment),
}
and projector = {
  id: Id.t,
  kind,
  syntax: list(piece),
  model: string,
};

// This is for comment insertion
let mk_secondary = (id, content) => [Secondary({id, content})];

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);
};

module Tile = {
  exception Empty_tile;
  type t = tile;
};

module Piece = {
  type t = piece;
};

module Segment = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = segment;
};

module Ancestor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    label: Label.t,
    mold: Mold.t,
    shards: (list(int), list(int)),
    children: (list(segment), list(segment)),
  };
};

module Siblings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Segment.t, Segment.t);
};

module Relatives = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type generation = (Ancestor.t, Siblings.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type ancestors = list(generation);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    siblings: Siblings.t,
    ancestors,
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type buffer =
    //| Parsed
    | Unparsed;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Normal
    | Buffer(buffer);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    focus: Direction.t,
    content: segment,
    mode,
  };
};

module Backpack = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Selection.t);
};

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type zipper = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret: Caret.t,
};
