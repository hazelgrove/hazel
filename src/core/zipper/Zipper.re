open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type caret =
  | Outer
  | Inner(int, int);

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret,
  caret_col_target: int,
};

let init: int => t =
  id => {
    selection: {
      focus: Left,
      content: [],
    },
    backpack: [],
    relatives: {
      siblings: ([], [Grout({id, shape: Convex})]),
      ancestors: [],
    },
    caret: Outer,
    caret_col_target: 0,
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (t, IdGen.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type chunkiness =
  | ByChar
  | MonoByChar
  | ByToken;

[@deriving (show({with_path: false}), sexp, yojson)]
type planar =
  | Up
  | Down
  | Left(chunkiness)
  | Right(chunkiness);

let from_plane: planar => Direction.t =
  fun
  | Left(_) => Left
  | Right(_) => Right
  | Up => Left
  | Down => Right;

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let parent = (z: t): option(Piece.t) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let sibs_with_sel =
    (
      {
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : Siblings.t =>
  switch (focus) {
  | Left => (l_sibs, content @ r_sibs)
  | Right => (l_sibs @ content, r_sibs)
  };

let pop_backpack = (z: t) =>
  Backpack.pop(Relatives.local_incomplete_tiles(z.relatives), z.backpack);

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  siblings =>
    switch (Siblings.neighbors(siblings)) {
    | (Some(l), Some(r)) => (Piece.monotile(l), Piece.monotile(r))
    | (Some(l), None) => (Piece.monotile(l), None)
    | (None, Some(r)) => (None, Piece.monotile(r))
    | (None, None) => (None, None)
    };

let remold_regrout = (d: Direction.t, z: t): IdGen.t(t) => {
  assert(Selection.is_empty(z.selection));
  open IdGen.Syntax;
  let+ relatives = Relatives.regrout(d, Relatives.remold(z.relatives));
  {...z, relatives};
};
