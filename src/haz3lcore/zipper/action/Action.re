open Util;

open Zipper;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type piece_goal =
  | Grout;

let of_piece_goal =
  fun
  | Grout => (
      fun
      | Piece.Grout(_) => true
      | _ => false
    );

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type goal =
  | Point(Point.t)
  | Piece(piece_goal, Direction.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type move =
  | Extreme(planar)
  | Local(planar)
  | Goal(goal);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type jump_target =
  | TileId([@equal (_, _) => true] Id.t)
  | BindingSiteOfIndicatedVar;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type rel =
  | Current
  | Id([@equal (_, _) => true] Id.t, Direction.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type select =
  | All
  | Resize(move)
  | Smart(int)
  | Tile(rel)
  | Term(rel);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type chooser('p_kind) =
  | Specific('p_kind)
  | ChooseLivelit;

/* This type defines the top-level actions used to manage
 * projectors,as distinguished from external_action,
 * which defines the actions available internally to all projectors,
 * and from each projector's own internal action type */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type project('p_kind, 'p_m, 'p_a) =
  | SetIndicated(chooser('p_kind)) /* Project syntax at caret */
  | RemoveIndicated /* Remove projector at caret */
  | Perform(Id.t, 'p_a) /* Set serialized projector model */
  | MoveCaretTo(Id.t) /* Move parent splice caret to projector */
  | Escape(Id.t, Direction.t); /* Pass control to parent editor */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type agent =
  | TyDi
  | LLM(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type buffer =
  | Set(agent)
  | Clear
  | Accept;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paste('p) =
  | String(string)
  | Segment(Segment.t('p));

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('p_kind, 'p, 'p_a) =
  | Reparse
  | Buffer(buffer)
  | Paste(paste('p))
  | Copy
  | Cut
  | Project(project('p_kind, 'p, 'p_a))
  | Move(move)
  | Jump(jump_target)
  | Select(select)
  | Unselect(option(Direction.t))
  | Destruct(Direction.t)
  | Insert(string)
  | RotateBackpack
  | MoveToBackpackTarget(planar)
  | Pick_up
  | Put_down
  | Introduce;

module Failure = Failure;

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let is_edit: t('k, 'p, 'a) => bool =
  fun
  | Paste(_)
  | Cut
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down
  | Introduce
  | Buffer(Accept | Clear | Set(_)) => true
  | Copy
  | Move(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false
  | Project(p) =>
    switch (p) {
    | Perform(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Escape(_)
    | MoveCaretTo(_) => false
    };

/* Determines whether undo/redo skips action */
let is_historic: t('k, 'p, 'a) => bool =
  fun
  | Copy
  | Move(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false
  | Cut
  | Buffer(Accept | Clear | Set(_))
  | Paste(_)
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down
  | Introduce => true
  | Project(p) =>
    switch (p) {
    | Perform(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Escape(_)
    | MoveCaretTo(_) => false
    };

let prevent_in_read_only_editor = (a: t('k, 'p, 'a)) => {
  switch (a) {
  | Copy
  | Move(_)
  | Unselect(_)
  | Jump(_)
  | Select(_) => false
  | Buffer(Set(_) | Accept | Clear)
  | Cut
  | Paste(_)
  | Reparse
  | Destruct(_)
  | Insert(_)
  | Pick_up
  | Put_down
  | RotateBackpack
  | MoveToBackpackTarget(_)
  | Introduce => true
  | Project(p) =>
    switch (p) {
    | Perform(_) => true // TODO: let projecors decide whether this is allowed
    | SetIndicated(_)
    | RemoveIndicated
    | Escape(_)
    | MoveCaretTo(_) => false
    }
  };
};

/* Currently animations are disabled during drag selection
 * to paper over a weird interaction with scroll-to-caret.
 * There is assuredly a better way to handle it but the
 * approaches I tried weren't wholly successful. */
let should_animate: t('k, 'p, 'a) => bool =
  fun
  | Select(s) =>
    switch (s) {
    | Resize(_) => false
    | All
    | Smart(_)
    | Tile(_)
    | Term(_) => true
    }
  | Unselect(_)
  | Paste(_)
  | Cut
  | Reparse
  | Insert(_)
  | Introduce
  | Destruct(_)
  | Pick_up
  | Put_down
  | Buffer(Accept | Clear | Set(_))
  | Copy
  | Move(_)
  | Jump(_)
  | RotateBackpack
  | MoveToBackpackTarget(_)
  | Project(_) => true;

let should_scroll_active: t('k, 'p, 'a) => bool =
  fun
  | Move(_)
  | Jump(_)
  | Select(Resize(_) | Term(_) | Smart(_) | Tile(_))
  | Destruct(_)
  | Insert(_)
  | Pick_up
  | Put_down
  | RotateBackpack
  | MoveToBackpackTarget(_)
  | Buffer(Set(_) | Accept | Clear)
  | Paste(_)
  | Copy
  | Cut
  | Reparse
  | Introduce => true
  | Project(_)
  | Unselect(_)
  | Select(All) => false;
