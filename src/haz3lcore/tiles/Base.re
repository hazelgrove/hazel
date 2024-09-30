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

  let decrement: t => t =
    fun
    | Outer
    | Inner(_, 0) => Outer
    | Inner(d, c) => Inner(d, c - 1);

  let offset: t => int =
    fun
    | Outer => 0
    | Inner(_, c) => c + 1;
};

module Tile = {
  exception Empty_tile;
  type t = tile;

  let split_shards = (id, label, mold, shards) =>
    shards |> List.map(i => {id, label, mold, shards: [i], children: []});

  let to_piece = t => Tile(t);

  let disintegrate = ({id, label, mold, shards, _}: t): list(tile) => {
    split_shards(id, label, mold, shards);
  };

  // postcond: output segment is nonempty
  let disassemble = ({id, label, mold, shards, children}: t): segment => {
    let shards = split_shards(id, label, mold, shards);
    Aba.mk(shards, children)
    |> Aba.join(s => [to_piece(s)], Fun.id)
    |> List.concat;
  };

  let pop_l = (tile: t): (piece, segment) =>
    disassemble(tile)
    |> ListUtil.split_first_opt
    |> OptUtil.get_or_raise(Empty_tile);
  let pop_r = (tile: t): (segment, piece) =>
    disassemble(tile)
    |> ListUtil.split_last_opt
    |> OptUtil.get_or_raise(Empty_tile);
};

module Piece = {
  type t = piece;

  let pop_l = (p: t): (t, segment) =>
    switch (p) {
    | Tile(t) => Tile.pop_l(t)
    | Grout(_)
    | Secondary(_)
    | Projector(_) => (p, [])
    };
  let pop_r = (p: t): (segment, t) =>
    switch (p) {
    | Tile(t) => Tile.pop_r(t)
    | Grout(_)
    | Secondary(_)
    | Projector(_) => ([], p)
    };
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

  let pop = (sel: t): option((piece, t)) =>
    switch (sel.focus, sel.content, ListUtil.split_last_opt(sel.content)) {
    | (_, [], _)
    | (_, _, None) => None
    | (Left, [p, ...content], _) =>
      let (p, rest) = Piece.pop_l(p);
      Some((p, {...sel, content: rest @ content}));
    | (Right, _, Some((content, p))) =>
      let (rest, p) = Piece.pop_r(p);
      Some((p, {...sel, content: content @ rest}));
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
