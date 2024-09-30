open Util;

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type selection_buffer =
  //| Parsed
  | Unparsed;

[@deriving (show({with_path: false}), sexp, yojson)]
type selection_mode =
  | Normal
  | Buffer(selection_buffer);

module Measured = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type measurement = {
    origin: Point.t,
    last: Point.t,
  };

  module Rows = {
    include IntMap;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type shape = {
      indent: Point.col,
      max_col: Point.col,
    };
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = IntMap.t(shape);
  };

  module Shards = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type shard = (int, measurement);
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = list(shard);
  };

  // indentation relative to container
  [@deriving (show({with_path: false}), sexp, yojson)]
  type rel_indent = int;
  // indentation relative to code container
  [@deriving (show({with_path: false}), sexp, yojson)]
  type abs_indent = int;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    tiles: Id.Map.t(Shards.t),
    grout: Id.Map.t(measurement),
    secondary: Id.Map.t(measurement),
    projectors: Id.Map.t(measurement),
    rows: Rows.t,
    linebreaks: Id.Map.t(rel_indent),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector)
and ancestor = {
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: (list(int), list(int)),
  children: (list(segment), list(segment)),
}
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
  kind: ProjectorKind.t,
  syntax: list(piece),
  model: string,
}
and zipper = {
  relatives,
  selection,
  backpack,
  caret: Caret.t,
}
and editor = {
  state,
  history,
  read_only: bool,
}
and state = {
  zipper,
  [@opaque]
  meta,
}
and affix = list((ActionBase.t(segment), state))
and history = (affix, affix)

and siblings = (segment, segment)
and generation' = (ancestor, siblings)
and ancestors' = list(generation')
and relatives = {
  siblings,
  ancestors: ancestors',
}
and selection = {
  focus: Direction.t,
  content: segment,
  mode: selection_mode,
}
and backpack = list(selection)
and meta = {
  col_target: int,
  statics: cached_statics,
  syntax: cached_syntax,
}
and cached_syntax = {
  segment,
  measured: Measured.t,
  tiles,
  holes: list(Grout.t),
  selection_ids: list(Id.t),
  term: UExp.t,
  /* This term, and the term-derived data structured below, may differ
   * from the term used for semantics. These terms are identical when
   * the backpack is empty. If the backpack is non-empty, then when we
   * make the term for semantics, we attempt to empty the backpack
   * according to some simple heuristics (~ try to empty it greedily
   * while moving rightwards from the current caret position).
   * this is currently necessary to have the cursorinfo/completion
   * workwhen the backpack is nonempty.
   *
   * This is a brittle part of the current implementation. there are
   * some other comments at some of the weakest joints; the biggest
   * issue is that dropping the backpack can add/remove grout, causing
   * certain ids to be present/non-present unexpectedly. */
  term_ranges,
  terms: TermMap.t,
  projectors: Id.Map.t(projector),
}
and range = (piece, piece)
and term_ranges = Id.Map.t(range)
and tiles = Id.Map.t(tile)
and cached_statics = {
  term: UExp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
};
// This is for comment insertion
let mk_secondary = (id, content) => [Secondary({id, content})];

module Tile = {
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
  type t = ancestor;
};

module Siblings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = siblings;
};

module Relatives = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type generation = generation';

  [@deriving (show({with_path: false}), sexp, yojson)]
  type ancestors = ancestors';

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = relatives;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = selection;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type buffer = selection_buffer;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode = selection_mode;
};

module Backpack = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = backpack;
};

module Zipper = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = zipper;
};

module TermRanges = {
  type t = term_ranges;
};

module TileMap = {
  type t = tiles;
};

module Meta = {
  type t = meta;
  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");
};

module Editor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = editor;
};
