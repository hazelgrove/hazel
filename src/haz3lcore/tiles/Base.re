open Util;

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
  kind: ProjectorKind.t,
  syntax: list(piece),
  model: string,
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

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);
};

module Zipper = {
  // assuming single backpack, shards may appear in selection, backpack, or siblings
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    relatives: Relatives.t,
    selection: Selection.t,
    backpack: Backpack.t,
    caret: Caret.t,
  };
};

module type MetaType = {
  type t;
  module type S;
};

module type HistoryType = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
};

module type StateType = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
};

module CachedStatics = {
  type t = {
    term: UExp.t,
    info_map: Statics.Map.t,
    error_ids: list(Id.t),
  };
};

module TermRanges = {
  include Id.Map;
  type range = (Piece.t, Piece.t);
  type nonrec t = t(range);
};

module TileMap = {
  include Id.Map;
  type t = Id.Map.t(Tile.t);
};

module Measured = {
  module Point = Point;
  open Util;
  open Point;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type measurement = {
    origin: Point.t,
    last: Point.t,
  };

  module Rows = {
    include IntMap;
    type shape = {
      indent: col,
      max_col: col,
    };
    type t = IntMap.t(shape);
  };

  module Shards = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type shard = (int, measurement);
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = list(shard);
  };

  // indentation relative to container
  type rel_indent = int;
  // indentation relative to code container
  type abs_indent = int;

  type t = {
    tiles: Id.Map.t(Shards.t),
    grout: Id.Map.t(measurement),
    secondary: Id.Map.t(measurement),
    projectors: Id.Map.t(measurement),
    rows: Rows.t,
    linebreaks: Id.Map.t(rel_indent),
  };
};

module CachedSyntax = {
  type t = {
    segment: Segment.t,
    measured: Measured.t,
    tiles: TileMap.t,
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
    term_ranges: TermRanges.t,
    terms: TermMap.t,
    projectors: Id.Map.t(projector),
  };
};

module Meta = {
  type t = {
    col_target: int,
    statics: CachedStatics.t,
    syntax: CachedSyntax.t,
  };
  module type S = {
    let measured: Measured.t;
    let term_ranges: TermRanges.t;
    let col_target: int;
  };
  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");
};

module State = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    zipper: Zipper.t,
    [@opaque]
    meta: Meta.t,
  };
};

module History = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type affix = list((ActionBase.t(Segment.t), State.t));
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (affix, affix);
};

module Editor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    state: State.t,
    history: History.t,
    read_only: bool,
  };
};
